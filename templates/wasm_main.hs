{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)
import GHC.Wasm.Prim (JSString(..), fromJSString)
import qualified Data.Text as T

import Harpe.Core (updateDOM, getDomEnv)
import App (Model, stepRenderWire, initModel, DomEnv)

foreign import javascript unsafe "__ghc_wasm_jsffi_jsval_manager.getJSVal($1)"
  js_get_string :: Int -> JSString

{-# NOINLINE appStore #-}
appStore :: IORef Model
appStore = unsafePerformIO (newIORef initModel)

renderFromWire :: Maybe T.Text -> IO ()
renderFromWire mbRaw = do
  model <- readIORef appStore
  env <- getDomEnv
  case stepRenderWire env mbRaw model of
    Left err -> updateDOM "app" ("<pre class='app-error'>" <> err <> "</pre>")
    Right (nextModel, htmlString, ioAction) -> do
      writeIORef appStore nextModel
      updateDOM "app" htmlString
      ioAction

foreign export ccall "app_init" appInit :: IO ()
appInit :: IO ()
appInit = renderFromWire (Just "MsgParent Init")

foreign export ccall "app_dispatch" appDispatch :: Int -> IO ()
appDispatch :: Int -> IO ()
appDispatch msgId = do
  let jsStr = js_get_string msgId
      raw = fromJSString jsStr
  renderFromWire (Just (T.pack raw))

main :: IO ()
main = return ()
