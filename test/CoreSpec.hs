{-# LANGUAGE OverloadedStrings #-}
module CoreSpec (runCoreTests) where

import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef)
import Harpe.Core
import TestHelper (assertEqual)

runCoreTests :: IO ()
runCoreTests = do
  putStrLn "\n--- Testing Harpe.Core ---"
  assertEqual "wrapMsg simple" "MsgParent (Clicked)" (wrapMsg ["MsgParent"] "Clicked")
  assertEqual "wrapMsg nested" "MsgParent (ChildMsg (Clicked))" (wrapMsg ["MsgParent", "ChildMsg"] "Clicked")

  -- default_effects_ctx with IORef tracking (tinkle = no-op)
  ref <- newIORef []
  let handler Nothing = Nothing
      handler _ = Just ([], return ())
      noTinkle _ = return ()
  _ <- default_effects_ctx ref noTinkle handler (Just "test")
  ctx <- readIORef ref
  assertEqual "default_effects_ctx updates IORef" [] ctx

  -- default_effects_ctx: state transition with cleanup (IDs differ, tinkle called)
  ref2 <- newIORef ["oldId"]
  let handler2 (Just _) = Just (["newId"], return ())
      handler2 _ = Nothing
      testTinkle _ = return ()
  _ <- default_effects_ctx ref2 testTinkle handler2 (Just "test")
  ctx2 <- readIORef ref2
  assertEqual "default_effects_ctx transitions to new IDs" ["newId"] ctx2

  -- default_effects_ctx: same IDs (no cleanup triggered)
  ref3 <- newIORef ["sameId"]
  let handler3 (Just _) = Just (["sameId"], return ())
      handler3 _ = Nothing
  _ <- default_effects_ctx ref3 noTinkle handler3 (Just "test")
  ctx3 <- readIORef ref3
  assertEqual "default_effects_ctx same IDs stays" ["sameId"] ctx3

  -- default_effects_ctx: Nothing match (no change to IORef)
  ref4 <- newIORef ["persistent"]
  let handler4 _ = Nothing
  _ <- default_effects_ctx ref4 noTinkle handler4 (Just "test")
  ctx4 <- readIORef ref4
  assertEqual "default_effects_ctx Nothing match keeps IORef" ["persistent"] ctx4

  -- default_effects_ctx: tinkle called on transition
  ref5 <- newIORef ["target"]
  tinkleCalled <- newIORef False
  let handler5 (Just _) = Just (["newId"], return ())
      handler5 _ = Nothing
      trackingTinkle "target" = writeIORef tinkleCalled True
      trackingTinkle _ = return ()
  _ <- default_effects_ctx ref5 trackingTinkle handler5 (Just "test")
  called <- readIORef tinkleCalled
  assertEqual "default_effects_ctx calls tinkle on transition" True called

  -- default_effects_ctx: multiple old IDs, tinkle called for each
  ref6 <- newIORef ["id1", "id2"]
  tinkleCalledIds <- newIORef []
  let handler6 (Just _) = Just (["newId"], return ())
      handler6 _ = Nothing
      multiTinkle id = modifyIORef tinkleCalledIds (\ids -> id : ids)
  _ <- default_effects_ctx ref6 multiTinkle handler6 (Just "test")
  calledIds <- readIORef tinkleCalledIds
  assertEqual "tinkle called for id1" True ("id1" `elem` calledIds)
  assertEqual "tinkle called for id2" True ("id2" `elem` calledIds)
  assertEqual "tinkle called exactly 2 times" 2 (length calledIds)

  -- default_effects_ctx: correct ID string passed to tinkle
  ref7 <- newIORef ["testListener"]
  capturedId <- newIORef ""
  let handler7 (Just _) = Just (["newId"], return ())
      handler7 _ = Nothing
      captureTinkle id = writeIORef capturedId id
  _ <- default_effects_ctx ref7 captureTinkle handler7 (Just "test")
  gotId <- readIORef capturedId
  assertEqual "tinkle receives correct ID string" "testListener" gotId

  -- default_effects_ctx: no tinkle call when IDs are the same (no transition)
  ref8 <- newIORef ["sameId"]
  sameTinkleCalled <- newIORef False
  let handler8 (Just _) = Just (["sameId"], return ())
      handler8 _ = Nothing
      sameTinkle _ = writeIORef sameTinkleCalled True
  _ <- default_effects_ctx ref8 sameTinkle handler8 (Just "test")
  sameCalled <- readIORef sameTinkleCalled
  assertEqual "tinkle NOT called when IDs unchanged" False sameCalled

  -- default_effects_ctx: transition to empty IDs (cleanup everything)
  ref9 <- newIORef ["oldListener", "oldTimer"]
  clearedIds <- newIORef []
  let handler9 (Just _) = Just ([], return ())
      handler9 _ = Nothing
      clearTinkle id = modifyIORef clearedIds (\ids -> id : ids)
  _ <- default_effects_ctx ref9 clearTinkle handler9 (Just "test")
  cleared <- readIORef clearedIds
  assertEqual "tinkle called for both old IDs on empty transition" True ("oldListener" `elem` cleared && "oldTimer" `elem` cleared)
  ctx9 <- readIORef ref9
  assertEqual "IORef updated to empty after transition" [] ctx9
