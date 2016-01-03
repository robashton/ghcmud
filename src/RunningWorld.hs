module RunningWorld
(
  createRunningWorld,
  --sendCommand,
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
data GenericSuccess = GenericSuccess

data ActiveRoom = ActiveRoom {
  activeRoomCoordinate :: Coordinate,
  activeRoomPlayers :: [PlayerId]
} deriving (Show)

data GameState = GameState {
  gameWorld :: WorldDefinition,
  activeRooms :: Map.Map Coordinate ActiveRoom,
  activePlayers :: Map.Map PlayerId Player
  } deriving (Show)

createRunningWorld :: WorldDefinition -> IO RunningWorld
createRunningWorld world = RunningWorld <$> (newMVar $ (GameState world mempty mempty))

-- This looks something like something...
withStateAndResult :: RunningWorld -> (GameState -> (a, GameState)) -> IO a
withStateAndResult (RunningWorld m) fn = do
  (result, newState) <- fn <$> takeMVar m
  putMVar m newState
  return result

addPlayer :: RunningWorld -> Player -> IO (Either FailFeedback GenericSuccess)
addPlayer world = withStateAndResult world . maybeSetupPlayer

maybeSetupPlayer :: Player -> GameState -> (Either FailFeedback GenericSuccess, GameState)
maybeSetupPlayer player initialState =
  either failure success $ playerIntoWorld player =<< (ensureRoomActive (playerLocation player) initialState) where
    failure reason = (Left reason, initialState)
    success s = (Right GenericSuccess, stateWithPlayer s)
    stateWithPlayer s = s { activePlayers = Map.insert (playerId player) player (activePlayers s) }

ensureRoomActive :: Coordinate -> GameState -> Either FailFeedback GameState
ensureRoomActive coordinate originalState =
  maybe makeRoomActive roomAlreadyActive $ Map.lookup coordinate currentActiveRooms where
    roomAlreadyActive _ = Right originalState
    makeRoomActive = maybe (Left RoomDoesNotExist) amendStateWithRoom $ Map.lookup coordinate (worldRooms (gameWorld originalState))
    amendStateWithRoom room = (Right originalState { activeRooms = Map.insert coordinate newActiveRoom currentActiveRooms })
    newActiveRoom = ActiveRoom coordinate []
    currentActiveRooms = (activeRooms originalState)

playerIntoWorld :: Player -> GameState -> Either FailFeedback GameState
playerIntoWorld player originalState =
  maybe failure success $ Map.lookup coordinate currentActiveRooms where
    failure = Left RoomDoesNotExist
    success room = Right originalState { activeRooms = Map.insert coordinate (updateRoomWithPlayer room player) currentActiveRooms }
    currentActiveRooms = (activeRooms originalState)
    coordinate = (playerLocation player)
    updateRoomWithPlayer room player = room { activeRoomPlayers = (playerId player) : (activeRoomPlayers room) }

--sendCommand :: RunningWorld -> PlayerId -> Command -> IO String
--sendCommand (RunningWorld m) c =
--  do
--    (result, newState) <- ((handleCommand c) <$> takeMVar m)
--    putMVar m newState
---    return result
--
--handleCommand :: Command -> GameState -> (String, GameState)
--handleCommand command state@( GameState { gameSession = session, gameWorld = world }) =
--  either handleError handleSuccess $ processCommand command world session where
--    handleError err = (translateCommandError err, state)
--    handleSuccess (feedback, newSession) = (feedback, state { gameSession = newSession })
--
--translateCommandError :: FailFeedback -> String
--translateCommandError RoomDoesNotExist = "There is no room there, doofus"
--

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


