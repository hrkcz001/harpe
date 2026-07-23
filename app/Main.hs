module Main where

import System.Environment (getArgs, getExecutablePath)
import System.FilePath (takeBaseName, replaceExtension, (</>), takeExtension, takeDirectory)
import System.Directory (listDirectory, doesDirectoryExist, createDirectoryIfMissing, doesFileExist)
import System.IO (withFile, IOMode(..), hSetEncoding, utf8, hGetContents, hPutStr, hPutStrLn, stderr, stdout)
import System.IO.Error (isUserError, ioeGetErrorString)
import System.Exit (exitFailure)
import Data.Char (toUpper, toLower, isSpace)
import Data.List (find, isPrefixOf)
import Control.Monad (forM_)
import Control.Exception (try, IOException)
import Harpe.Parser (parseTemplate)
import Harpe.Compiler (compileTemplate)

readUtf8File :: FilePath -> IO String
readUtf8File path = withFile path ReadMode $ \h -> do
  hSetEncoding h utf8
  content <- hGetContents h
  mapM_ (\c -> c `seq` pure ()) content
  pure content

writeUtf8File :: FilePath -> String -> IO ()
writeUtf8File path content = withFile path WriteMode $ \h -> do
  hSetEncoding h utf8
  hPutStr h content

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

-- | Format an IOException for user display.
-- GHC wraps 'ioError (userError msg)' as "user error (msg)" — strip that prefix
-- so harpe's own error codes and hints are shown cleanly.
formatIOError :: IOException -> String
formatIOError e
  | isUserError e = ioeGetErrorString e
  | otherwise     = show e

main :: IO ()
main = do
  -- Ensure both stdout and stderr use UTF-8 so harpe error messages
  -- render correctly on Windows (which defaults to CP1252).
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  args <- getArgs
  case args of
    ["-i", inputDir, "-o", outputDir] -> compileFolder inputDir outputDir
    [inputFile] -> compileFile inputFile
    _ -> do
      putStrLn "Usage:"
      putStrLn "  harpe -i <input-dir> -o <output-dir>"
      putStrLn "  harpe <input-file.harpe>"
      exitFailure

compileFolder :: FilePath -> FilePath -> IO ()
compileFolder inputDir outputDir = do
  isDir <- doesDirectoryExist inputDir
  if not isDir
    then do
      putStrLn $ "Input directory does not exist: " ++ inputDir
      exitFailure
    else do
      createDirectoryIfMissing True outputDir
      files <- listDirectory inputDir
      let harpeFiles = filter (\f -> takeExtension f == ".harpe") files
      if null harpeFiles
        then putStrLn $ "No .harpe files found in " ++ inputDir
        else do
          forM_ harpeFiles $ \file -> do
            let inputFile = inputDir </> file
                moduleName = capitalize (takeBaseName file)
                outputFile = outputDir </> replaceExtension file "hs"
            compileFileTo inputFile outputFile moduleName
          let hasApp = any (\f -> takeBaseName f == "App") harpeFiles
          if hasApp
            then do
              writeMainHs outputDir
              writeMainJs outputDir inputDir
            else return ()

-- | Locate the templates directory. Looks next to the executable first
-- (installed layout: <prefix>/bin/harpe, <prefix>/share/harpe/templates),
-- then falls back to a sibling `templates/` dir for `cabal run` / dev use.
findTemplatesDir :: IO FilePath
findTemplatesDir = do
  exePath <- getExecutablePath
  -- installed: <bindir>/harpe  →  <sharedir>/harpe-*/templates (best-effort)
  let exeDir = takeDirectory exePath
      -- cabal run layout: dist-newstyle/.../harpe  →  harpe/templates
      devCandidates =
        [ exeDir </> "templates"
        , exeDir </> ".." </> "templates"
        , exeDir </> ".." </> ".." </> "templates"
        , exeDir </> ".." </> ".." </> ".." </> "templates"
        ]
  found <- findFirst devCandidates
  case found of
    Just d  -> pure d
    Nothing -> do
      putStrLn "Warning: could not locate harpe templates directory, using './templates' as fallback"
      pure "templates"
  where
    findFirst [] = pure Nothing
    findFirst (d:ds) = do
      exists <- doesDirectoryExist d
      if exists then pure (Just d) else findFirst ds

writeMainHs :: FilePath -> IO ()
writeMainHs outputDir = do
  templatesDir <- findTemplatesDir
  let templatePath = templatesDir </> "wasm_main.hs"
      mainPath     = outputDir </> "Main.hs"
  templateExists <- doesFileExist templatePath
  if not templateExists
    then do
      putStrLn $ "Error: template not found at " ++ templatePath
      exitFailure
    else do
      mainContent <- readUtf8File templatePath
      writeUtf8File mainPath mainContent
      putStrLn $ "Generated Main.hs in " ++ mainPath

compileFileTo :: FilePath -> FilePath -> String -> IO ()
compileFileTo inputFile outputFile moduleName = do
  putStrLn $ "Compiling " ++ inputFile ++ " to module " ++ moduleName ++ "..."
  content <- readUtf8File inputFile
  case parseTemplate content of
    Left err -> do
      hPutStrLn stderr ""
      hPutStrLn stderr $ "harpe: parse error in " ++ inputFile ++ ":"
      hPutStrLn stderr err
      exitFailure
    Right ast -> do
      result <- try (compileTemplate inputFile moduleName ast) :: IO (Either IOException String)
      case result of
        Left ioErr -> do
          hPutStrLn stderr ""
          hPutStrLn stderr $ "harpe: compile error in " ++ inputFile ++ ":"
          hPutStrLn stderr $ formatIOError ioErr
          exitFailure
        Right compiledCode -> do
          writeUtf8File outputFile compiledCode
          putStrLn $ "  -> " ++ outputFile

compileFile :: FilePath -> IO ()
compileFile inputFile = do
  let moduleName = capitalize (takeBaseName inputFile)
      outputFile = replaceExtension inputFile "hs"
  compileFileTo inputFile outputFile moduleName

writeMainJs :: FilePath -> FilePath -> IO ()
writeMainJs outputDir inputDir = do
  wasmName     <- findProjectWasmName inputDir
  templatesDir <- findTemplatesDir
  let templatePath = templatesDir </> "wasm_main.js"
      mainJsPath   = outputDir </> "main.js"
  templateExists <- doesFileExist templatePath
  if not templateExists
    then do
      putStrLn $ "Error: template not found at " ++ templatePath
      exitFailure
    else do
      template <- readUtf8File templatePath
      let mainJsContent = replaceSubstr "{{WASM_NAME}}" wasmName template
      writeUtf8File mainJsPath mainJsContent
      putStrLn $ "Generated main.js in " ++ mainJsPath
  where
    -- Simple non-regex substitution of the first occurrence
    replaceSubstr :: String -> String -> String -> String
    replaceSubstr _   _   [] = []
    replaceSubstr old new s@(c:cs)
      | old `isPrefixOf` s = new ++ replaceSubstr old new (drop (length old) s)
      | otherwise          = c   : replaceSubstr old new cs

findProjectWasmName :: FilePath -> IO String
findProjectWasmName inputDir = do
  mbName <- findCabalName inputDir
  case mbName of
    Just n -> pure (n ++ ".wasm")
    Nothing -> do
      mbParentName <- findCabalName (takeDirectory inputDir)
      case mbParentName of
        Just n -> pure (n ++ ".wasm")
        Nothing -> pure "app.wasm"
  where
    findCabalName dir = do
      exists <- doesDirectoryExist dir
      if not exists
        then pure Nothing
        else do
          files <- listDirectory dir
          let cabalFiles = filter (\f -> takeExtension f == ".cabal") files
          case cabalFiles of
            [] -> pure Nothing
            (cabalFile:_) -> do
              content <- readUtf8File (dir </> cabalFile)
              let lines' = lines content
                  nameLine = find (\l -> "name:" `isPrefixOf` dropWhile isSpace (map toLower l)) lines'
              case nameLine of
                Just l ->
                  let val = drop 1 (dropWhile (/= ':') l)
                  in pure (Just (trim val))
                Nothing -> pure Nothing

    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
