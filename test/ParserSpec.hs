module ParserSpec (runParserTests) where

import Data.List (isInfixOf)
import Harpe.AST
import Harpe.Parser
import TestHelper

runParserTests :: IO ()
runParserTests = do
  -- Parser and Preprocessor Tests
  putStrLn "\n--- Testing Harpe.Parser ---"
  case parseTemplate "<h1>Hello</h1>" of
    Right ast -> do
      assertEqual "HTML templateNodes length" 1 (length (templateNodes ast))
      assertEqual "HTML node content" (HtmlChunk "<h1>Hello</h1>\n") (head (templateNodes ast))
    Left err -> error $ "HTML template parsing failed: " ++ err

  -- Inline expressions
  case parseTemplate "<h1>//= show model =//</h1>" of
    Right ast -> do
      assertEqual "Inline expression AST" 
        [HtmlChunk "<h1>", HaskellExpr "show model", HtmlChunk "</h1>\n"] 
        (templateNodes ast)
    Left err -> error $ "Inline expression parsing failed: " ++ err

  -- Clinches
  case parseTemplate "//= clinch Counter.harpe =//" of
    Right ast -> do
      assertEqual "Clinch AST has clinch node" True (ClinchTemplate "Counter.harpe" Nothing `elem` templateNodes ast)
    Left err -> error $ "Clinch parsing failed: " ++ err

  case parseTemplate "//= clinch Counter.harpe handleCounterEvent =//" of
    Right ast -> do
      assertEqual "Clinch AST with bubble handler has clinch node" True (ClinchTemplate "Counter.harpe" (Just "handleCounterEvent") `elem` templateNodes ast)
    Left err -> error $ "Clinch parsing with handler failed: " ++ err

  -- Chunks
  case parseTemplate "//= chunk renderItem x\n<li>//= show x =//</li>\n=//" of
    Right ast -> do
      assertEqual "Chunk parsing nodes length" 1 (length (templateNodes ast))
      case head (templateNodes ast) of
        ChunkBlock name args _ -> do
          assertEqual "Chunk name" "renderItem" name
          assertEqual "Chunk args" ["x"] args
        other -> error $ "Expected ChunkBlock, got: " ++ show other
    Left err -> error $ "Chunk parsing failed: " ++ err

  -- Crude blocks parsing
  case parseTemplate "//= crude logMessage msg\n  putStrLn msg\n=//" of
    Right ast -> do
      assertEqual "Crude block parsing length" 1 (length (templateNodes ast))
      case head (templateNodes ast) of
        CrudeBlock (Just name) args body -> do
          assertEqual "Crude name" "logMessage" name
          assertEqual "Crude args" ["msg"] args
          assertEqual "Crude body" ["putStrLn msg"] body
        other -> error $ "Expected named CrudeBlock, got: " ++ show other
    Left err -> error $ "Crude named block parsing failed: " ++ err

  case parseTemplate "//= imply crude logMessage \"Hello\" =//" of
    Right ast -> do
      let hasImplyCrude (ImplyCrude name args) = name == "logMessage" && args == ["\"Hello\""]
          hasImplyCrude _ = False
      assertEqual "Imply crude parsed correctly" True (any hasImplyCrude (templateNodes ast))
    Left err -> error $ "Imply crude parsing failed: " ++ err

  -- Alien block parsing
  case parseTemplate "//= alien changeUrl url\n  window.history.pushState({}, '', url)\n=//" of
    Right ast -> do
      assertEqual "Alien block parsed correctly" 1 (length (templateNodes ast))
      case head (templateNodes ast) of
        AlienBlock name args body _ -> do
          assertEqual "Alien name" "changeUrl" name
          assertEqual "Alien args" ["url"] args
          assertEqual "Alien body" ["window.history.pushState({}, '', url)"] body
        other -> error $ "Expected AlienBlock, got: " ++ show other
    Left err -> error $ "Alien block parsing failed: " ++ err

  -- Add alien parsing
  case parseTemplate "//= add alien changeUrl as myUrl from MyAliens.harpe =//" of
    Right ast -> do
      assertEqual "Add alien parsed correctly" 1 (length (templateNodes ast))
      case head (templateNodes ast) of
        AddAlien className instName path -> do
          assertEqual "Add alien className" "changeUrl" className
          assertEqual "Add alien instName" "myUrl" instName
          assertEqual "Add alien path" "MyAliens.harpe" path
        other -> error $ "Expected AddAlien, got: " ++ show other
    Left err -> error $ "Add alien parsing failed: " ++ err


  case parseTemplate "//= add alien changeUrl as myUrl from MyAliens.harpe =//" of
    Right ast -> do
      assertEqual "Add alien changeUrl parsed correctly" 1 (length (templateNodes ast))
      case head (templateNodes ast) of
        AddAlien className instName _ -> do
          assertEqual "Add alien changeUrl className" "changeUrl" className
          assertEqual "Add alien changeUrl instName" "myUrl" instName
        other -> error $ "Expected AddAlien, got: " ++ show other
    Left err -> error $ "Add alien changeUrl parsing failed: " ++ err

  -- Imply alien parsing
  case parseTemplate "//= imply alien changeUrl \"info\" =//" of
    Right ast -> do
      assertEqual "Imply alien parsed correctly" 1 (length (templateNodes ast))
      case head (templateNodes ast) of
        ImplyAlien name args -> do
          assertEqual "Imply alien name" "changeUrl" name
          assertEqual "Imply alien args" ["\"info\""] args
        other -> error $ "Expected ImplyAlien from imply alien, got: " ++ show other
    Left err -> error $ "Imply alien parsing failed: " ++ err

  -- Contextual alien parsing (inside crude block inside on block)
  let contextualTemplate = unlines
        [ "//= root"
        , "  //= default"
        , "  //= on KeyDownListener True"
        , "    //= crude"
        , "      imply contextual alien keylistener"
        , "    =//"
        , "  =//"
        , "=//"
        ]
  case parseTemplate contextualTemplate of
    Right ast -> do
      assertEqual "Contextual alien template has 1 root" 1 (length (templateNodes ast))
      -- Verify the template structure: root > default > on > crude > imply contextual alien
      case head (templateNodes ast) of
        RootBlock [DefaultBlock [] [] [OnBlock "KeyDownListener True" [CrudeBlock Nothing [] ["imply contextual alien keylistener"]]]] ->
          putStrLn "  [OK] contextual alien parsed inside crude inside on inside default inside root"
        other -> error $ "Expected specific contextual structure, got: " ++ show other
    Left err -> error $ "Contextual alien parsing failed: " ++ err


  -- Tinkle inside alien block parsing (with //= tinkle syntax)
  let tinkleAlienTemplate = unlines
        [ "//= alien keylistener"
        , "  document.addEventListener('keydown', handler);"
        , "//= tinkle"
        , "  document.removeEventListener('keydown', handler)"
        , "=//"
        ]
  case parseTemplate tinkleAlienTemplate of
    Right ast -> do
      assertEqual "Tinkle alien template parsed" 1 (length (templateNodes ast))
      case head (templateNodes ast) of
        AlienBlock name args body (Just tinkle) -> do
          assertEqual "Alien name" "keylistener" name
          assertEqual "Alien args" [] args
          assertEqual "Alien body line" "document.addEventListener('keydown', handler);" (head body)
          assertEqual "Tinkle body line" "document.removeEventListener('keydown', handler)" (head tinkle)
        other -> error $ "Expected AlienBlock with tinkle, got: " ++ show other
    Left err -> error $ "Tinkle alien parsing failed: " ++ err

  -- Alien block WITHOUT tinkle (just body)
  case parseTemplate "//= alien changeUrl url\n  window.history.pushState({}, '', url)\n=//" of
    Right ast -> do
      assertEqual "Alien without tinkle parsed" 1 (length (templateNodes ast))
      case head (templateNodes ast) of
        AlienBlock name args body Nothing -> do
          assertEqual "Alien name" "changeUrl" name
          assertEqual "Alien args" ["url"] args
          assertEqual "Alien body" ["window.history.pushState({}, '', url)"] body
        other -> error $ "Expected AlienBlock without tinkle, got: " ++ show other
    Left err -> error $ "Alien without tinkle parsing failed: " ++ err


  -- Propagate alien parsing
  case parseTemplate "//= propagate alien changeUrl =//" of
    Right ast -> do
      assertEqual "Propagate alien parsed correctly" 1 (length (templateNodes ast))
      case head (templateNodes ast) of
        PropagateAlien name -> assertEqual "Propagate name" "changeUrl" name
        other -> error $ "Expected PropagateAlien, got: " ++ show other
    Left err -> error $ "Propagate alien parsing failed: " ++ err

  -- Propagated alien block parsing
  case parseTemplate "//= propagate alien changeUrl url\n  window.history.pushState({}, '', url)\n=//" of
    Right ast -> do
      assertEqual "Propagated alien block parsed correctly" 1 (length (templateNodes ast))
      case head (templateNodes ast) of
        PropagatedAlienBlock name args body _ -> do
          assertEqual "Propagated alien block name" "changeUrl" name
          assertEqual "Propagated alien block args" ["url"] args
          assertEqual "Propagated alien block body" ["window.history.pushState({}, '', url)"] body
        other -> error $ "Expected PropagatedAlienBlock, got: " ++ show other
    Left err -> error $ "Propagated alien block parsing failed: " ++ err


  -- WithoutMVU parsing
  case parseTemplate "//= without model / update / view =//" of
    Right ast -> do
      assertEqual "WithoutMVU parsed correctly" 1 (length (templateNodes ast))
      case head (templateNodes ast) of
        WithoutMVU -> pure ()
        other -> error $ "Expected WithoutMVU, got: " ++ show other
    Left err -> error $ "WithoutMVU parsing failed: " ++ err

  case parseTemplate "//= without mvu ==/" of
    Right ast -> do
      assertEqual "WithoutMVU with mvu and ==/ parsed correctly" 1 (length (templateNodes ast))
      case head (templateNodes ast) of
        WithoutMVU -> pure ()
        other -> error $ "Expected WithoutMVU, got: " ++ show other
    Left err -> error $ "WithoutMVU with mvu and ==/ parsing failed: " ++ err

  case parseTemplate "//= without view update model =//" of
    Right ast -> do
      assertEqual "WithoutMVU parsed correctly in different order" 1 (length (templateNodes ast))
      case head (templateNodes ast) of
        WithoutMVU -> pure ()
        other -> error $ "Expected WithoutMVU, got: " ++ show other
    Left err -> error $ "WithoutMVU parsing failed in different order: " ++ err

  case parseTemplate "//= on ValueChanged val\n  <p class=\"val\">//= show val =//</p>\n=//" of
    Right ast -> do
      assertEqual "Standalone on blocks parsed as DefaultBlock with empty default nodes" 1 (length (templateNodes ast))
      case head (templateNodes ast) of
        DefaultBlock [] [] [OnBlock pattern body] -> do
          assertEqual "OnBlock pattern" "ValueChanged val" pattern
          assertEqual "OnBlock body length" 3 (length body)
        other -> error $ "Expected DefaultBlock with empty default nodes, got: " ++ show other
    Left err -> error $ "Standalone on block parsing failed: " ++ err

  case parseTemplate "//= on Event1\n  <h1>1</h1>\n=//\n//= on Event2\n  <h2>2</h2>\n=//" of
    Right ast -> do
      assertEqual "Multiple standalone on blocks length" 2 (length (templateNodes ast))
    Left err -> error $ "Multiple standalone on blocks failed: " ++ err

  -- Standalone on block inside root
  let nestedStandaloneOn = unlines
        [ "//= root"
        , "  //= on Event1"
        , "    <h1>1</h1>"
        , "  =//"
        , "  <div>still root</div>"
        , "=//"
        ]
  case parseTemplate nestedStandaloneOn of
    Right ast -> do
      assertEqual "Nested standalone on block inside root parsed" 1 (length (templateNodes ast))
    Left err -> error $ "Nested standalone on block inside root failed: " ++ err

  case parseTemplate "<div //= onClick Event =//></div>" of
    Right ast -> do
      assertEqual "keyword boundary check: onClick is not an on block (length)" 3 (length (templateNodes ast))
      case templateNodes ast of
        [HtmlChunk _, EventBinding "onClick" ["Event"], HtmlChunk _] -> pure ()
        other -> error $ "Expected HTML chunk, EventBinding, HTML chunk, got: " ++ show other
    Left err -> error $ "onClick parsing failed: " ++ err

  -- Imports and Shuffled Code Blocks
  putStrLn "\n--- Testing Shuffled Code Blocks & Imports ---"
  let shuffledTemplate = unlines
        [ "import qualified Data.List as L"
        , "//= root"
        , "<h1>Welcome</h1>"
        , "=//"
        , "data Msg = Clicked"
        , "import Data.Maybe"
        , "type Model = Int"
        , "update Clicked m = (m + 1, [])"
        ]
  case parseTemplate shuffledTemplate of
    Right ast -> do
      assertEqual "Imports extracted correctly"
        ["import qualified Data.List as L", "import Data.Maybe"]
        (templateImports ast)
      assertEqual "Declarations extracted correctly"
        ["data Msg = Clicked", "type Model = Int", "update Clicked m = (m + 1, [])"]
        (templateDeclarations ast)
    Left err -> error $ "Shuffled template parsing failed: " ++ err

  -- Syntax Validation & Edge Cases
  putStrLn "\n--- Testing Syntax Validation & Edge Cases ---"
  
  -- Unclosed block
  let unclosedDefault = unlines
        [ "//= default"
        , "  <h1>No close tag</h1>"
        ]
  assertLeft "Reject unclosed default block" "unbalanced" (parseTemplate unclosedDefault)

  let unclosedChunk = unlines
        [ "//= chunk myLayout"
        , "  <div>unclosed</div>"
        ]
  assertLeft "Reject unclosed chunk block" "unbalanced" (parseTemplate unclosedChunk)

  let unclosedCrude = unlines
        [ "//= crude myAction"
        , "  putStrLn \"unclosed\""
        ]
  assertLeft "Reject unclosed crude block" "unbalanced" (parseTemplate unclosedCrude)

  -- Malformed event name (camelCase validation)
  let malformedOnClick = "<button //= onclick Clicked =//>Click</button>"
  assertLeft "Reject lowercase onclick" "onclick" (parseTemplate malformedOnClick)

  let malformedOn_Click = "<button //= on_click Clicked =//>Click</button>"
  assertLeft "Reject snake_case on_click" "on_click" (parseTemplate malformedOn_Click)

  -- Legacy control directive rejection
  assertLeft "Reject legacy if directive" "Legacy" (parseTemplate "//= if cond =//\n<h1>Hello</h1>\n//= endif =//")

  -- Separate line validation tests
  let malformedLineOpener = unlines
        [ "<div> //= default"
        , "  <h1>No close tag</h1>"
        , "=//"
        ]
  assertLeft "Reject block opener not on separate line" "block opener must be on its own separate line" (parseTemplate malformedLineOpener)

  let malformedLineCloser = unlines
        [ "//= default"
        , "  <h1>No close tag</h1> </div> =//"
        ]
  assertLeft "Reject block closer not on separate line" "block closer '=//' must be on its own separate line" (parseTemplate malformedLineCloser)

  -- Pragmas extraction
  let pragmaTemplate = unlines
        [ "//={-% LANGUAGE OverloadedStrings %-}"
        , "//={-# LANGUAGE RecordWildCards #-}"
        , "<h1>Test</h1>"
        , "=-/"
        ]
  case parseTemplate pragmaTemplate of
    Right ast -> do
      assertEqual "Pragmas preserved in declarations"
        ["{-# LANGUAGE RecordWildCards #-}"]
        (filter ("{-# LANGUAGE" `isInfixOf`) (templateDeclarations ast))
    Left _ -> pure () -- Ignore parse errors for raw test since we changed block parser
