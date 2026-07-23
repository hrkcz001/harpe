module CompilerSpec (runCompilerTests) where

import Data.IORef (newIORef)
import Data.List (isInfixOf)
import Harpe.AST
import Harpe.Compiler
import Harpe.Compiler.Common
import Harpe.Compiler.Alien
import Harpe.Compiler.NodeUtils (extractModelType, compileCrudeDecl, partitionNodes, PartitionedNodes(..))
import Harpe.Compiler.PatternValidator (validatePattern)
import qualified Harpe.Compiler.ViewGen as ViewGen
import qualified Harpe.Compiler.EffectsGen as EffectsGen
import Harpe.Parser
import TestHelper

runCompilerTests :: IO ()
runCompilerTests = do
  putStrLn "\n--- Testing Harpe.Compiler ---"
  assertEqual "toLowerFirst" "counter" (toLowerFirst "Counter")
  let includeInfo = includeInfoFromPath "folder/SubComponent.harpe" Nothing
  assertEqual "includeModuleName" "SubComponent" (includeModuleName includeInfo)
  assertEqual "includeFieldName" "subComponent" (includeFieldName includeInfo)
  assertEqual "extractModelType type Model" (Just "Int") (extractModelType "type Model = Int")
  assertEqual "extractModelType type State" (Just "String") (extractModelType "type State = String")

  blockRef <- newIORef 0
  ctxAccum <- newIORef []
  ctxRef <- newIORef 0

  -- EventBinding compilation tests (including static parenthesized arguments)
  compiledSimple <- ViewGen.compileNode blockRef "Test" "" [] [] (EventBinding "onClick" ["Clicked"])
  assertEqual "compile simple EventBinding" "writeHtml (\" onclick=\\\"\" <> dispatchJS (msgWrappers ++ [\"MsgParent\"]) \"Clicked\" [] <> \"\\\"\")" compiledSimple

  compiledParam <- ViewGen.compileNode blockRef "Test" "" [] [] (EventBinding "onClick" ["(NavigateTo", "\"info\")"])
  assertEqual "compile parenthesized EventBinding" "writeHtml (\" onclick=\\\"\" <> dispatchJS (msgWrappers ++ [\"MsgParent\"]) \"NavigateTo \\\"info\\\"\" [] <> \"\\\"\")" compiledParam

  -- Crude compilation tests
  compiledAnonCrude <- EffectsGen.compileNode blockRef ctxAccum ctxRef "Test" "" [] ["changeUrl"] [] (CrudeBlock Nothing [] ["putStrLn \"Hello\"", "imply alien changeUrl \"info\""])
  assertEqual "compile anonymous CrudeBlock" "putStrLn \"Hello\" >>\n        changeUrl \"info\"" compiledAnonCrude

  assertThrowsIO "compile anonymous CrudeBlock with bare alien fails"
    "unsafe FFI call to alien"
    (EffectsGen.compileNode blockRef ctxAccum ctxRef "Test" "" [] ["changeUrl"] [] (CrudeBlock Nothing [] ["putStrLn \"Hello\"", "changeUrl \"info\""]))



  compiledImplyCrude <- EffectsGen.compileNode blockRef ctxAccum ctxRef "Test" "" [] [] [] (ImplyCrude "logMessage" ["\"Hello\""])
  assertEqual "compile imply crude" "logMessage \"Hello\"" compiledImplyCrude

  let namedCrudeDecl = compileCrudeDecl [] (CrudeBlock (Just "logMessage") ["msg"] ["putStrLn msg"])
  assertEqual "compile named CrudeBlock declaration" (Right "logMessage msg = do\n  putStrLn msg\n") namedCrudeDecl

  assertLeft "compile named CrudeBlock with bare alien fails"
    "unsafe FFI call to alien"
    (compileCrudeDecl ["changeUrl"] (CrudeBlock (Just "logMessage") ["msg"] ["changeUrl msg"]))

  -- Contextual alien in crude block
  compiledCtxCrude <- EffectsGen.compileNode blockRef ctxAccum ctxRef "Test" "" [] ["keylistener"] [] (CrudeBlock Nothing [] ["imply contextual alien keylistener"])
  assertEqual "compile contextual alien CrudeBlock" "Harpe.Core.harpe_ctx_guard \"keylistener\" (keylistener)" compiledCtxCrude

  compiledMultiOnBlock <- ViewGen.compileNode blockRef "Test" "" [] [] (DefaultBlock [] [HtmlChunk "test"] [OnBlock "Event1 _ / Evnt2 / Evnt3 _" [HtmlChunk "body"]])
  assertEqual "compile multi-event OnBlock" "default_view \"harpe-block-Test-0\" (writeHtml \"test\") (\\mb -> on_view (\\event -> case event of { EventParent (Event1 _) -> Just (writeHtml \"body\", \"on\"); EventParent (Evnt2) -> Just (writeHtml \"body\", \"on\"); EventParent (Evnt3 _) -> Just (writeHtml \"body\", \"on\"); _ -> Nothing }) mb) activeEvent" compiledMultiOnBlock

  compiledStandaloneOnBlock <- ViewGen.compileNode blockRef "Test" "" [] [] (DefaultBlock [] [] [OnBlock "Event1" [HtmlChunk "body"]])
  assertEqual "compile standalone OnBlock" "default_view \"harpe-block-Test-1\" (return ()) (\\mb -> on_view (\\event -> case event of { EventParent (Event1) -> Just (writeHtml \"body\", \"on\"); _ -> Nothing }) mb) activeEvent" compiledStandaloneOnBlock

  -- DefaultBlock without contextual aliens (uses default_effects)
  compiledCtxDefaultBlock <- EffectsGen.compileNode blockRef ctxAccum ctxRef "Test" "" [] [] []
    (DefaultBlock ["RedirectTo \"home\""]
      [HtmlChunk "<div>home</div>"]
      [OnBlock "RedirectTo \"info\"" [HtmlChunk "<div>info</div>"]])
  assertEqual "compile DefaultBlock without contextual uses old API" True
    ("default_effects" `isInfixOf` compiledCtxDefaultBlock && not ("default_effects_ctx" `isInfixOf` compiledCtxDefaultBlock))

  -- DefaultBlock WITH contextual aliens (uses default_effects_ctx + tinkle)
  compiledCtxBlock <- EffectsGen.compileNode blockRef ctxAccum ctxRef "Test" "" [] ["keylistener"] ["keylistener"]
    (DefaultBlock [] []
      [OnBlock "KeyDownListener True" [CrudeBlock Nothing [] ["imply contextual alien keylistener"]]])
  assertEqual "compile DefaultBlock with contextual uses default_effects_ctx" True
    ("default_effects_ctx" `isInfixOf` compiledCtxBlock)
  assertEqual "compile DefaultBlock with contextual includes IORef name" True
    ("ctxTracker_Test_0" `isInfixOf` compiledCtxBlock)
  assertEqual "compile DefaultBlock with contextual includes tinkle lambda" True
    ("keylistener_tinkle" `isInfixOf` compiledCtxBlock)
  assertEqual "compile DefaultBlock with contextual includes string pattern" True
    ("\"keylistener\"" `isInfixOf` compiledCtxBlock)
  -- Verify accumulator contains the IORef declaration
  ctxDecls <- EffectsGen.ctxAccumFetch ctxAccum
  assertEqual "ctxAccum contains IORef declaration" True (any ("ctxTracker_Test_0" `isInfixOf`) ctxDecls)

  -- Alien with tinkle: compileAlien generates both keylistener and keylistener_tinkle FFIs
  let compiledAlienWithTinkle = compileAlien (AlienBlock "keylistener" [] ["document.addEventListener('keydown', eventHandler)"] (Just ["document.removeEventListener('keydown', eventHandler)"]))
  assertEqual "compileAlien with tinkle includes keylistener" True ("keylistener" `isInfixOf` compiledAlienWithTinkle)
  assertEqual "compileAlien with tinkle includes keylistener_tinkle" True ("keylistener_tinkle" `isInfixOf` compiledAlienWithTinkle)

  -- Alien WITHOUT tinkle: only the main function
  let compiledAlienNoTinkle = compileAlien (AlienBlock "changeUrl" ["url"] ["window.history.pushState({}, '', //= url =//)"] Nothing)
  assertEqual "compileAlien without tinkle has changeUrl" True ("changeUrl" `isInfixOf` compiledAlienNoTinkle)
  assertEqual "compileAlien without tinkle no _tinkle" False ("_tinkle" `isInfixOf` compiledAlienNoTinkle)

  -- PropagatedAlienBlock with tinkle: partitionNodes converts to AlienBlock + PropagateAlien
  let propBlocks = partitionNodes [PropagatedAlienBlock "wsConn" [] ["ws.connect(url)"] (Just ["ws.close()"])]
  assertEqual "partitionNodes converts PropagatedAlienBlock" 1 (length (pLocalAliens propBlocks))
  case pLocalAliens propBlocks of
    [AlienBlock name args body (Just tinkle)] -> do
      assertEqual "PropagatedAlienBlock name preserved" "wsConn" name
      assertEqual "PropagatedAlienBlock tinkle body" "ws.close()" (head tinkle)
    other -> error $ "Expected AlienBlock with tinkle from PropagatedAlienBlock, got: " ++ show other
  -- Also verify the PropagateAlien directive was emitted
  assertEqual "partitionNodes emits PropagateAlien" True (not (null (pProps propBlocks)))
  -- compileAlien works on the converted AlienBlock
  let compiledProp = compileAlien (AlienBlock "wsConn" [] ["ws.connect(url)"] (Just ["ws.close()"]))
  assertEqual "compileAlien on propagated alien with tinkle" True ("wsConn_tinkle" `isInfixOf` compiledProp)

  -- Explicit tinkle call via crude: keylistener_tinkle is not an alien name, passes through unguarded
  compiledTinkleCall <- EffectsGen.compileNode blockRef ctxAccum ctxRef "Test" "" [] [] []
    (CrudeBlock Nothing [] ["keylistener_tinkle"])
  assertEqual "explicit tinkle call via crude" "keylistener_tinkle" compiledTinkleCall

  -- Multiple contextual aliens with partial tinkle coverage
  -- keylistener has tinkle, inputfield does not
  compiledMultiCtx <- EffectsGen.compileNode blockRef ctxAccum ctxRef "Test" "" [] ["keylistener", "inputfield"] ["keylistener"]
    (DefaultBlock [] []
      [OnBlock "BothActive" [CrudeBlock Nothing [] ["imply contextual alien keylistener", "imply contextual alien inputfield"]]])
  assertEqual "multi-contextual uses default_effects_ctx" True ("default_effects_ctx" `isInfixOf` compiledMultiCtx)
  assertEqual "multi-contextual includes keylistener_tinkle" True ("keylistener_tinkle" `isInfixOf` compiledMultiCtx)
  assertEqual "multi-contextual does NOT include inputfield_tinkle" False ("inputfield_tinkle" `isInfixOf` compiledMultiCtx)
  assertEqual "multi-contextual includes keylistener string pattern" True ("\"keylistener\"" `isInfixOf` compiledMultiCtx)
  assertEqual "multi-contextual includes keylistener ID" True ("\"keylistener\"" `isInfixOf` compiledMultiCtx)
  assertEqual "multi-contextual includes inputfield ID" True ("\"inputfield\"" `isInfixOf` compiledMultiCtx)

  let compiledAlien = compileAlien (AlienBlock "changeUrl" ["url"] ["window.history.pushState({}, '', //= url =//)"] Nothing)
      expectedAlien = "#if defined(wasm32_HOST_ARCH)\nforeign import javascript unsafe \"window.history.pushState({}, '', $1)\"\n  js_changeUrl :: JSString -> IO ()\n\nchangeUrl :: String -> IO ()\nchangeUrl url = js_changeUrl (toJSString url)\n#else\nchangeUrl :: String -> IO ()\nchangeUrl _ = return ()\n#endif\n"
  assertEqual "compileAlien" expectedAlien compiledAlien

  assertThrowsIO "compileNode ImplyAlien fails outside of crude blocks"
    "not allowed outside of crude blocks"
    (ViewGen.compileNode blockRef "Test" "" [] [] (ImplyAlien "changeUrl" ["\"info\""]))

  let testAst = TemplateAST [] ["type Event = InfoExt.Event"] [RootBlock [HtmlChunk "test"]]
  compiledStr <- compileTemplate "test.harpe" "Test" testAst
  assertEqual "compileTemplate type Event alias" True ("type ParentEvent = InfoExt.Event" `isInfixOf` compiledStr)

  let withoutAst = TemplateAST [] [] [WithoutMVU, AlienBlock "changeUrl" ["url"] ["window.history.pushState({}, '', //= url =//)"] Nothing]
  compiledWithout <- compileTemplate "test.harpe" "Test" withoutAst
  assertEqual "compileTemplate without MVU contains changeUrl FFI" True ("changeUrl" `isInfixOf` compiledWithout)
  assertEqual "compileTemplate without MVU does not contain Component" False ("instance Component" `isInfixOf` compiledWithout)

  -- Strict Pattern Matching & default / on
  putStrLn "\n--- Testing Strict Pattern Matching ---"
  
  -- Test default / on parsing
  case parseTemplate "//= default / on KeyDownListener False / on RedirectTo \"home\"\n  <h1>Home</h1>\n=//" of
    Right ast -> do
      case templateNodes ast of
        [DefaultBlock ["KeyDownListener False", "RedirectTo \"home\""] _ _] -> putStrLn "  [OK] parse default / on KeyDownListener / on RedirectTo"
        other -> error $ "Expected DefaultBlock, got: " ++ show other
    Left err -> error $ "parse default / on KeyDownListener failed: " ++ err

  -- Test strict pattern matches (validatePattern)
  assertEqual "validatePattern constant string" (Right ()) (validatePattern "RedirectTo \"info\"")
  assertEqual "validatePattern wildcard" (Right ()) (validatePattern "EventCaptured _")
  assertEqual "validatePattern flick" (Right ()) (validatePattern "flick")
  assertEqual "validatePattern simple var" (Right ()) (validatePattern "EventCaptured desc")
  assertEqual "validatePattern typed var" (Right ()) (validatePattern "EventCaptured (desc :: String)")
  assertEqual "validatePattern nested constructor" (Right ()) (validatePattern "EventCounter (Counter.EventParent Clicked)")
  
  -- Test validation failures
  assertLeft "validatePattern reject double type sig" "invalid argument"
    (case validatePattern "EventCaptured (desc :: String :: Int)" of { Left err -> Left err; Right _ -> Right () })

  assertLeft "validatePattern reject lowercase ctor" "Constructor must start with an uppercase letter"
    (case validatePattern "eventCaptured desc" of { Left err -> Left err; Right _ -> Right () })

  -- Test Event1 key / Event2 key compilation
  compiledSharedVar <- ViewGen.compileOnBlock blockRef "Test" "" [] [] (OnBlock "Event1 key / Event2 key" [HtmlChunk "body"])
  assertEqual "compile multi-event shared var binder" 
    "EventParent (Event1 key) -> Just (writeHtml \"body\", \"on\"); EventParent (Event2 key) -> Just (writeHtml \"body\", \"on\"); " 
    compiledSharedVar
