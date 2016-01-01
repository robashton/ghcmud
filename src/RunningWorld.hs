module RunningWorld
(
  createRunningWorld,
  sendCommand,
  RunningWorld
) where

import World
import Parser
import Session

import Control.Concurrent
import Control.Monad
import Data.Either

data RunningWorld = RunningWorld (MVar GameState)

data GameState = GameState {
  gameWorld :: World,
  gameSession :: Session
  } deriving (Show)

createRunningWorld :: World -> Either FailFeedback (IO RunningWorld)
createRunningWorld world = liftM RunningWorld <$> newMVar <$> (GameState world) <$> sessionStart (Coordinate 0 0) createPlayer world

sendCommand :: RunningWorld -> Command -> IO String
sendCommand (RunningWorld m) c =
  do
    (result, newState) <- ((handleCommand c) <$> takeMVar m)
    putMVar m newState
    return result

handleCommand :: Command -> GameState -> (String, GameState)
handleCommand command state@( GameState { gameSession = session, gameWorld = world }) =
  either handleError handleSuccess $ processCommand command world session where
    handleError err = (translateCommandError err, state)
    handleSuccess (feedback, newSession) = (feedback, state { gameSession = newSession })

translateCommandError :: FailFeedback -> String
translateCommandError RoomDoesNotExist = "There is no room there, doofus"

createPlayer :: Player
createPlayer =
  Player {
    playerHealth = 10,
    playerLevel = 1,
    playerExperience = 0
  }

-- WorldDefinition
  -- Contains descriptions of rooms
  -- The default starting location within that world
  -- monsters, items, etc to be found on those monsters and descriptions of all of that

-- ActiveWorld
  -- Contains room "instances" which are created on demand (when a player needs to go into the room)
  -- This will have the effect of spawning anything in that room that might need spawning
  -- We'll have a tick that goes through active instances and prunes any that haven't been used in a while
  -- We'll have queued up actions in those worlds too and they'll be executed either on that tick or a different tick
  -- Same tick will likely re-spawn monsters if a room has been active for too long for example


