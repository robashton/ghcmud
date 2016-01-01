module RunningWorld
(
  createRunningWorld,
  sendCommand,
  addPlayer,
  RunningWorld,
) where

import WorldDefinition
import Parser
import Session

import Control.Concurrent
import Control.Monad
import Data.Either
import qualified Data.Map as Map

data RunningWorld = RunningWorld (MVar GameState)
data PlayerId = String deriving (Show)
data GenericSuccess = GenericSuccess

data ActiveRoom = ActiveRoom {
  activeRoomCoordinate :: Coordinate,
  activeRoomPlayers :: [PlayerId]
} deriving (Show)

data GameState = GameState {
  gameWorld :: WorldDefinition,
  activeRooms :: Map.Map Coordinate ActiveRoom,
  activePlayers :: Map.Map String Player
  } deriving (Show)

createRunningWorld :: WorldDefinition -> Either FailFeedback (IO RunningWorld)
createRunningWorld world = liftM RunningWorld <$> newMVar <$> (GameState world mempty mempty)

addPlayer :: RunningWorld -> Player -> IO (Either FailFeedback GenericSuccess)
addPlayer (RunningWorld m) player =
  do
    GenericSuccess <*> (putMVar m) <$> (maybeSetupPlayer player) <$> takeMVar m

maybeSetupPlayer :: Player -> GameState -> Either FailFeedback GameState
maybeSetupPlayer _ s = Right s

--sendCommand :: RunningWorld -> PlayerId -> Command -> IO String
--sendCommand (RunningWorld m) c =
--  do
--    (result, newState) <- ((handleCommand c) <$> takeMVar m)
--    putMVar m newState
--    return result

--handleCommand :: Command -> GameState -> (String, GameState)
--handleCommand command state@( GameState { gameSession = session, gameWorld = world }) =
--  either handleError handleSuccess $ processCommand command world session where
--    handleError err = (translateCommandError err, state)
--    handleSuccess (feedback, newSession) = (feedback, state { gameSession = newSession })
--
--translateCommandError :: FailFeedback -> String
--translateCommandError RoomDoesNotExist = "There is no room there, doofus"
--
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


