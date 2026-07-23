-- | Harpe.CodeGen.Names — naming conventions for generated Haskell code.
--
-- Every identifier that the Harpe compiler emits into generated @.hs@ files
-- is defined here.  Compiler.hs imports these instead of building names with
-- raw string literals, so that renaming a convention is a one-line change.
--
-- Design: this is a /leaf/ module — it must not import any other Harpe.* module.
module Harpe.CodeGen.Names where

import Data.Char (toLower)

-- ---------------------------------------------------------------------------
-- Parent-layer type and value names
-- (hoisted from the template; renamed with "Parent" prefix in generated code)
-- ---------------------------------------------------------------------------

nmParentMsg        :: String
nmParentMsg        = "ParentMsg"

nmParentEvent      :: String
nmParentEvent      = "ParentEvent"

nmParentModel      :: String
nmParentModel      = "ParentModel"

nmParentUpdate     :: String
nmParentUpdate     = "parentUpdate"

nmInitParentModel  :: String
nmInitParentModel  = "initParentModel"

-- | Placeholder constructor when the template defines no Msg type.
nmParentMsgDummy   :: String
nmParentMsgDummy   = "ParentMsgDummy"

-- | Placeholder constructor when the template defines no Event type.
nmParentEventDummy :: String
nmParentEventDummy = "ParentEventDummy"

-- ---------------------------------------------------------------------------
-- Fixed constructors in the wrapper Msg / Event types
-- ---------------------------------------------------------------------------

nmMsgParent   :: String
nmMsgParent   = "MsgParent"

nmMsgAlien    :: String
nmMsgAlien    = "MsgAlien"

nmEventParent :: String
nmEventParent = "EventParent"

nmEventAlien  :: String
nmEventAlien  = "EventAlien"

-- ---------------------------------------------------------------------------
-- Module-derived naming helpers
-- (functions of the module name, e.g. "Counter")
-- ---------------------------------------------------------------------------

-- | Wrapper Msg type for a module: @"App" -> "AppMsg"@.
mkWrapperMsgName :: String -> String
mkWrapperMsgName modName = modName ++ "Msg"

-- | Wrapper Event type for a module: @"App" -> "AppEvent"@.
mkWrapperEventName :: String -> String
mkWrapperEventName modName = modName ++ "Event"

-- | Msg constructor for a child module: @"Counter" -> "MsgCounter"@.
mkMsgCtor :: String -> String
mkMsgCtor modName = "Msg" ++ modName

-- | Event constructor for a child module: @"Counter" -> "EventCounter"@.
mkEventCtor :: String -> String
mkEventCtor modName = "Event" ++ modName

-- | Field name for an included child model: @"Counter" -> "counter"@.
-- (Lower-cases the first character.)
mkFieldName :: String -> String
mkFieldName []     = []
mkFieldName (c:cs) = toLower c : cs

-- | Record field name for a child model: @"Counter" -> "counterModel"@.
mkFieldModelName :: String -> String
mkFieldModelName modName = mkFieldName modName ++ "Model"

-- ---------------------------------------------------------------------------
-- Fixed generated declaration names
-- ---------------------------------------------------------------------------

nmModel           :: String
nmModel           = "Model"

nmInitModel       :: String
nmInitModel       = "initModel"

nmUpdate          :: String
nmUpdate          = "update"

nmDecodeMsg       :: String
nmDecodeMsg       = "decodeMsg"

nmStepRenderWire  :: String
nmStepRenderWire  = "stepRenderWire"

nmRunWireMessages :: String
nmRunWireMessages = "runWireMessages"

nmDomEnv          :: String
nmDomEnv          = "DomEnv"

-- ---------------------------------------------------------------------------
-- Runtime variable names (used inside generated view / effects bodies)
-- ---------------------------------------------------------------------------

-- | The composed model value passed to @view@ / @effects@.
rtComposedModel :: String
rtComposedModel = "composedModel"

-- | The active event passed to @view@ / @effects@.
rtActiveEvent :: String
rtActiveEvent = "activeEvent"

-- | The message-wrapper stack passed to @view@ / @effects@.
rtMsgWrappers :: String
rtMsgWrappers = "msgWrappers"

-- | Local @model@ binding inside the view let-block.
rtModel :: String
rtModel = "model"

-- | @parentModel@ field accessor / local binding.
rtParentModel :: String
rtParentModel = "parentModel"

-- ---------------------------------------------------------------------------
-- DOM identifier convention
-- ---------------------------------------------------------------------------

-- | Prefix for DOM element IDs managed by Harpe DefaultBlocks.
domBlockPrefix :: String
domBlockPrefix = "harpe-block-"

-- | Build a unique block DOM id: @mkBlockId "App" 0 == "harpe-block-App-0"@.
mkBlockId :: String -> Int -> String
mkBlockId modName idx = domBlockPrefix ++ modName ++ "-" ++ show idx

-- ---------------------------------------------------------------------------
-- Tinkle convention
-- ---------------------------------------------------------------------------

-- | Suffix appended to an alien name to create its tinkle cleanup function.
nmTinkleSuffix :: String
nmTinkleSuffix = "_tinkle"
