-- Harpe-generated boilerplate for MVU module {{MODULE_NAME}}
-- Placeholders replaced at compile time by the Harpe compiler:
--   {{MODULE_NAME}}   – e.g. App
--   {{WRAPPER_MSG}}   – e.g. AppMsg
--   {{WRAPPER_EVENT}} – e.g. AppEvent

decodeMsg :: T.Text -> Either Err {{WRAPPER_MSG}}
decodeMsg = Harpe.Core.decodeMsg

stepRenderWire :: DomEnv -> Maybe T.Text -> Model -> Either Err (Model, T.Text, IO ())
stepRenderWire env Nothing model =
  let html = renderHtml env (Harpe.Core.view [] model Nothing)
      io = Harpe.Core.effects [] model Nothing
  in Right (model, html, io)
stepRenderWire env (Just raw) model = do
  msg <- {{MODULE_NAME}}.decodeMsg raw
  let (nextModel, events) = {{MODULE_NAME}}.update msg model
      activeEvent = case events of { [] -> Nothing; (e:_) -> Just e }
      html = renderHtml env (Harpe.Core.view [] nextModel activeEvent)
      io = Harpe.Core.effects [] nextModel activeEvent
  pure (nextModel, html, io)

runWireMessages :: DomEnv -> [T.Text] -> Model -> Either Err (Model, T.Text, IO ())
runWireMessages env raws model = do
  msgs <- traverse ({{MODULE_NAME}}.decodeMsg) raws
  (nextModel, events) <- runMessages msgs model
  let activeEvent = case events of { [] -> Nothing; (e:_) -> Just e }
      html = renderHtml env (Harpe.Core.view [] nextModel activeEvent)
      io = Harpe.Core.effects [] nextModel activeEvent
  pure (nextModel, html, io)

instance Component Model where
  type Msg Model = {{WRAPPER_MSG}}
  type Event Model = {{WRAPPER_EVENT}}
  update = {{MODULE_NAME}}.update
