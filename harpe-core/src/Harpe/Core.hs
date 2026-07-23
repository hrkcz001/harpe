{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
#if defined(wasm32_HOST_ARCH)
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
#endif

module Harpe.Core where

import Control.Monad (foldM)
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Data.Kind (Type)
import Data.List ((\\), intersect)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
#if defined(wasm32_HOST_ARCH)
import GHC.Wasm.Prim (JSString(..), toJSString, fromJSString)

foreign import javascript unsafe "document.getElementById($1).innerHTML = $2"
  js_update_dom :: JSString -> JSString -> IO ()

foreign import javascript unsafe "(() => { window.harpe_blocks = window.harpe_blocks || {}; for (const el of document.querySelectorAll('[id^=\"harpe-block-\"]')) { window.harpe_blocks[el.id] = { html: el.innerHTML, view: el.getAttribute('data-harpe-view') || '' }; } const res = []; for (const id in window.harpe_blocks) { const data = window.harpe_blocks[id]; res.push(id + '\\x1f' + data.html + '\\x1f' + data.view); } return res.join('\\x1e'); })()"
  js_get_dom_env :: IO JSString

#endif



type Err = T.Text

type DomEnv = [(T.Text, (T.Text, T.Text))]

updateDOM :: T.Text -> T.Text -> IO ()
#if defined(wasm32_HOST_ARCH)
updateDOM elementId htmlStr =
  js_update_dom (toJSString (T.unpack elementId)) (toJSString (T.unpack htmlStr))
#else
updateDOM _ _ = return ()
#endif

splitOn :: Char -> T.Text -> [T.Text]
splitOn c s = T.split (== c) s

parseDomEnv :: T.Text -> DomEnv
parseDomEnv raw =
  [ (id', (html', attr'))
  | record <- splitOn '\x1e' raw
  , let parts = splitOn '\x1f' record
  , length parts == 3
  , let id'   = parts !! 0
        html' = parts !! 1
        attr' = parts !! 2
  ]

getDomEnv :: IO DomEnv
#if defined(wasm32_HOST_ARCH)
getDomEnv = do
  jsVal <- js_get_dom_env
  let raw = T.pack (fromJSString jsVal)
  pure (parseDomEnv raw)
#else
getDomEnv = pure []
#endif

-- | Pure HTML builder monad (Builder for O(1) concat and environment reader)
newtype Html a = Html { runHtml :: DomEnv -> (a, B.Builder) }

instance Functor Html where
  fmap f (Html h) = Html $ \env -> let (x, b) = h env in (f x, b)

instance Applicative Html where
  pure x = Html $ \_ -> (x, mempty)
  Html hf <*> Html hx = Html $ \env ->
    let (f, b1) = hf env
        (x, b2) = hx env
    in (f x, b1 <> b2)

instance Monad Html where
  Html h >>= f = Html $ \env ->
    let (x, b1) = h env
        (y, b2) = runHtml (f x) env
    in (y, b1 <> b2)

writeHtml :: T.Text -> Html ()
writeHtml str = Html $ \_ -> ((), B.fromText str)

renderHtml :: DomEnv -> Html () -> T.Text
renderHtml env (Html h) = let ((), b) = h env in TL.toStrict (B.toLazyText b)

-- | Represents a reactive UI component in Model-View-Update architecture.
class (Show (Msg model), Read (Msg model), Show (Event model), Read (Event model)) => Component model where
  type Msg model :: Type
  type Event model :: Type

  update :: Msg model -> model -> (model, [Event model])
  view :: [T.Text] -> model -> Maybe (Event model) -> Html ()
  effects :: [T.Text] -> model -> Maybe (Event model) -> IO ()

-- | Runs a list of messages sequentially over the model, accumulating events.
runMessages :: Component model => [Msg model] -> model -> Either Err (model, [Event model])
runMessages msgs initialModel = foldM (\(m, accEvents) msg -> do
  let (nextModel, newEvents) = update msg m
  pure (nextModel, accEvents ++ newEvents)
  ) (initialModel, []) msgs

-- | Decodes a raw message string into the message type using standard Read.
decodeMsg :: Read msg => T.Text -> Either Err msg
decodeMsg raw =
  case readMaybe (T.unpack raw) of
    Just msg -> Right msg
    Nothing -> Left ("Unable to decode message: " <> raw)

-- | Recursively wraps a message string with constructor prefixes.
wrapMsg :: [T.Text] -> T.Text -> T.Text
wrapMsg wrappers msg = foldr (\w acc -> w <> " (" <> acc <> ")") msg wrappers

-- | Generates the javascript dispatch call for an event.
dispatchJS :: [T.Text] -> T.Text -> [T.Text] -> T.Text
dispatchJS wrappers ctor [] =
  escapeQuotes $ "dispatch('" <> wrapMsg wrappers ctor <> "')"
dispatchJS wrappers ctor inputs =
  let prefix = "dispatch('" <> foldr (\w acc -> w <> " (" <> acc) (ctor <> " \\\"' + ") wrappers
      jsInputs = mconcat (map (\i -> "encodeURIComponent(((document.querySelector('[name=\\'" <> i <> "\\']') || { value: '' }).value)) + '\\\" ' + ") inputs)
      suffix = "'" <> T.replicate (length wrappers) ")" <> "')"
  in escapeQuotes $ prefix <> jsInputs <> suffix

escapeQuotes :: T.Text -> T.Text
escapeQuotes = T.replace "\"" "&quot;"

-- | Helper class to automatically convert values to HTML String
class ToHtml a where
  toHtmlStr :: a -> T.Text

instance ToHtml T.Text where
  toHtmlStr = id

instance ToHtml String where
  toHtmlStr = T.pack

instance {-# OVERLAPPABLE #-} Show a => ToHtml a where
  toHtmlStr = T.pack . show

toHtml :: ToHtml a => a -> Html ()
toHtml val = writeHtml (toHtmlStr val)

-- | DOM-retained view updates for default/on blocks
default_view :: T.Text -> Html () -> (Maybe event -> Maybe (Html (), T.Text)) -> Maybe event -> Html ()
default_view blockId defHtml handler mbEvent = Html $ \env ->
  let
    mbState = lookup blockId env
    currentAttr = maybe "" snd mbState
    currentHtml = maybe "" fst mbState

    runH h = TL.toStrict (B.toLazyText (snd (runHtml h env)))

    result = case mbEvent of
      Just _ ->
        case handler mbEvent of
          Just (html, attr) ->
            "<div id=\"" <> blockId <> "\" data-harpe-view=\"" <> attr <> "\">" <> runH html <> "</div>"
          Nothing ->
            if currentAttr == "on"
              then "<div id=\"" <> blockId <> "\" data-harpe-view=\"on\">" <> (if T.null currentHtml then runH defHtml else currentHtml) <> "</div>"
              else "<div id=\"" <> blockId <> "\" data-harpe-view=\"default\">" <> runH defHtml <> "</div>"
      Nothing ->
        if currentAttr == "on"
          then "<div id=\"" <> blockId <> "\" data-harpe-view=\"on\">" <> (if T.null currentHtml then runH defHtml else currentHtml) <> "</div>"
          else "<div id=\"" <> blockId <> "\" data-harpe-view=\"default\">" <> runH defHtml <> "</div>"
  in ((), B.fromText result)

-- | Side effects updates for default/on blocks
default_effects :: (Maybe event -> Maybe (IO ())) -> Maybe event -> IO ()
default_effects handler mbEvent =
  case mbEvent of
    Just _ ->
      case handler mbEvent of
        Just io -> io
        Nothing -> return ()
    Nothing -> return ()  -- | Side effects updates with contextual alien lifecycle tracking.
-- Uses an IORef to track the PREVIOUS branch's contextual alien IDs.
-- When a transition occurs (IDs differ), calls 'tinkleFn' for each
-- previously-active ID to clean up, then runs the new branch's IO.
default_effects_ctx
  :: IORef [T.Text]
  -> (T.Text -> IO ())
  -> (Maybe event -> Maybe ([T.Text], IO ()))
  -> Maybe event -> IO ()
default_effects_ctx ref tinkleFn handler mbEvent = do
  prevCtx <- readIORef ref
  case mbEvent of
    Just _ -> case handler mbEvent of
      Just (currCtx, io) -> do
        let toTinkle = prevCtx \\ currCtx
        mapM_ tinkleFn toTinkle
        writeIORef ref currCtx
        
        oldActive <- readIORef harpe_active_prev_ctx
        let survived = prevCtx `intersect` currCtx
        writeIORef harpe_active_prev_ctx survived
        io
        writeIORef harpe_active_prev_ctx oldActive
      Nothing -> return ()
    Nothing -> return ()

{-# NOINLINE harpe_active_prev_ctx #-}
harpe_active_prev_ctx :: IORef [T.Text]
harpe_active_prev_ctx = unsafePerformIO (newIORef [])

harpe_ctx_guard :: T.Text -> IO () -> IO ()
harpe_ctx_guard name action = do
  prev <- readIORef harpe_active_prev_ctx
  if name `elem` prev
    then return ()
    else action

on_view :: (event -> Maybe (Html (), T.Text)) -> Maybe event -> Maybe (Html (), T.Text)
on_view handler mbEvent =
  case mbEvent of
    Nothing -> Nothing
    Just event -> handler event

on_effects :: (event -> Maybe (IO ())) -> Maybe event -> Maybe (IO ())
on_effects handler mbEvent =
  case mbEvent of
    Nothing -> Nothing
    Just event -> handler event

-- | Contextual-informer-aware variant of on_effects.
on_effects_ctx :: (event -> Maybe ([T.Text], IO ())) -> Maybe event -> Maybe ([T.Text], IO ())
on_effects_ctx handler mbEvent =
  case mbEvent of
    Nothing -> Nothing
    Just event -> handler event

imply :: a -> a
imply = id

-- | Bubbles up child component events to parent messages.
class Bubble parentMsg childEvent where
  bubble :: childEvent -> Maybe parentMsg

instance {-# OVERLAPPABLE #-} Bubble p c where
  bubble _ = Nothing
