module RunningWorld
(
  createRunningWorld,
  sendCommand,
  addPlayer,
  RunningWorld,
) where

import WorldDefinition
import WorldInstance

import Control.Concurrent

data RunningWorld = RunningWorld (MVar WorldInstance)

createRunningWorld :: WorldDefinition -> IO RunningWorld
createRunningWorld world = RunningWorld <$> (newMVar $ (WorldInstance world mempty mempty))

-- This looks something like something...
withStateAndResult :: RunningWorld -> (WorldInstance -> (a, WorldInstance)) -> IO a
withStateAndResult (RunningWorld m) fn = do
  (result, newState) <- fn <$> takeMVar m
  putMVar m newState
  return result

addPlayer :: RunningWorld -> Player -> IO (Either InstanceFailure GenericSuccess)
addPlayer world = withStateAndResult world . addPlayerToGame

sendCommand :: RunningWorld -> PlayerId -> Command -> IO (Either InstanceFailure String)
sendCommand world targetPlayerId command = withStateAndResult world $ processCommand targetPlayerId command
