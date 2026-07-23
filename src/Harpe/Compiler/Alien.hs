module Harpe.Compiler.Alien where

import Data.List (intercalate)
import Harpe.AST
import Harpe.CodeGen.Names (nmTinkleSuffix)
import Harpe.Compiler.Common (replaceSubstring)

replaceArgPlaceholders :: [String] -> String -> String
replaceArgPlaceholders args originalJs =
  let indexed :: [(Int, String)]
      indexed = zip [1..] args
  in foldl replaceOne originalJs indexed
  where
    replaceOne js (idx, arg) =
      let variations = [ "//=" ++ arg ++ "=//"
                       , "//= " ++ arg ++ " =//"
                       , "//=" ++ arg ++ " =//"
                       , "//= " ++ arg ++ "=//"
                       ]
          repl = "$" ++ show idx
      in foldl (\s var -> replaceSubstring var repl s) js variations

compileAlien :: TemplateNode -> String
compileAlien (AlienBlock name args bodyLines Nothing) =
  compileAlienCommon name args bodyLines ""
compileAlien (AlienBlock name args bodyLines (Just tinkleBody)) =
  let tinkleJS = intercalate "\n" tinkleBody
      tinkleName = name ++ nmTinkleSuffix
      tinkleFuncName = "js_" ++ tinkleName
      tinkleFFI = "foreign import javascript unsafe " ++ show tinkleJS ++ "\n  " ++
                  tinkleFuncName ++ " :: IO ()\n\n" ++
                  tinkleName ++ " :: IO ()\n" ++
                  tinkleName ++ " = " ++ tinkleFuncName ++ "\n"
      tinkleNative = tinkleName ++ " :: IO ()\n" ++
                     tinkleName ++ " = return ()\n"
      tinkleDecl = "#if defined(wasm32_HOST_ARCH)\n" ++ tinkleFFI ++ "\n#else\n" ++ tinkleNative ++ "\n#endif\n"
  in compileAlienCommon name args bodyLines tinkleDecl
compileAlien _ = ""

compileAlienCommon :: String -> [String] -> [String] -> String -> String
compileAlienCommon name args bodyLines extra =
  let jsCodeRaw = intercalate "\n" bodyLines
      jsCode = replaceArgPlaceholders args jsCodeRaw
      jsFuncName = "js_" ++ name
      numArgs = length args
      jsStringTypes = replicate numArgs "JSString"
      stringTypes = replicate numArgs "T.Text"
      
      jsSig = intercalate " -> " (jsStringTypes ++ ["IO ()"])
      hsSig = intercalate " -> " (stringTypes ++ ["IO ()"])
      
      jsImport = "foreign import javascript unsafe " ++ show jsCode ++ "\n  " ++ jsFuncName ++ " :: " ++ jsSig
      
      hsArgs = unwords args
      hsArgsStr = if null args then "" else " " ++ hsArgs
      hsBody = if numArgs == 0
                 then jsFuncName
                 else jsFuncName ++ " " ++ unwords (map (\a -> "(toJSString (T.unpack " ++ a ++ "))") args)
      
      wasmDecl = jsImport ++ "\n\n" ++ name ++ " :: " ++ hsSig ++ "\n" ++ name ++ hsArgsStr ++ " = " ++ hsBody
      
      dummyArgs = unwords (replicate numArgs "_")
      dummyArgsStr = if null args then "" else " " ++ dummyArgs
      nativeDecl = name ++ " :: " ++ hsSig ++ "\n" ++ name ++ dummyArgsStr ++ " = return ()"
  in "#if defined(wasm32_HOST_ARCH)\n" ++ wasmDecl ++ "\n#else\n" ++ nativeDecl ++ "\n#endif\n" ++ extra

nubByAlienName :: [TemplateNode] -> [TemplateNode]
nubByAlienName = go []
  where
    go _ [] = []
    go seen (AlienBlock name args body mbTinkle : rest)
      | name `elem` seen = go seen rest
      | otherwise = AlienBlock name args body mbTinkle : go (name : seen) rest

    go seen (_ : rest) = go seen rest
