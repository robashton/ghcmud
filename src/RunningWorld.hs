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
import qualified Data.Set as Set
import Data.Graph.Inductive.Query.Monad (mapFst)

data RunningWorld = RunningWorld (MVar GameState)
data GenericSuccess = GenericSuccess
  deriving (Show)

data ActiveRoom = ActiveRoom {
  activeRoomCoordinate :: Coordinate,
  activeRoomPlayers :: Set.Set PlayerId
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
  either failure success $ playerIntoWorld player =<< (maybeEnsureRoomActive (playerLocation player) initialState) where
    failure reason = (Left reason, initialState)
    success s = (Right GenericSuccess, stateWithPlayer s)
    stateWithPlayer s = s { activePlayers = Map.insert (playerId player) player (activePlayers s) }

maybeEnsureRoomActive :: Coordinate -> GameState -> Either FailFeedback GameState
maybeEnsureRoomActive coordinate originalState =
  maybe (Left RoomDoesNotExist) setupRoom $ findRoomDefinition coordinate originalState where
    setupRoom definition = Right $ snd $ ensureRoomActive definition originalState

ensureRoomActive :: RoomDefinition -> GameState -> (ActiveRoom, GameState)
ensureRoomActive room originalState =
  maybe makeRoomActive roomAlreadyActive $ Map.lookup coordinate currentActiveRooms where
    roomAlreadyActive room = (room, originalState)
    makeRoomActive = (newActiveRoom, originalState { activeRooms = Map.insert coordinate newActiveRoom currentActiveRooms })
    coordinate = (roomId room)
    currentActiveRooms = (activeRooms originalState)
    newActiveRoom = ActiveRoom coordinate mempty

playerIntoWorld :: Player -> GameState -> Either FailFeedback GameState
playerIntoWorld player originalState =
  maybe failure success $ Map.lookup coordinate currentActiveRooms where
    failure = Left RoomDoesNotExist
    success room = Right originalState { activeRooms = Map.insert coordinate (addPlayerToRoom (playerId player) room) currentActiveRooms }
    currentActiveRooms = (activeRooms originalState)
    coordinate = (playerLocation player)

addPlayerToRoom :: PlayerId -> ActiveRoom -> ActiveRoom
addPlayerToRoom targetPlayerId room =
  room { activeRoomPlayers = Set.insert targetPlayerId (activeRoomPlayers room) }

removePlayerFromRoom :: PlayerId -> ActiveRoom -> ActiveRoom
removePlayerFromRoom targetPlayerId room =
  room { activeRoomPlayers = Set.delete targetPlayerId (activeRoomPlayers room) }

data MovePlayerContext = MovePlayerContext {
  mpContextPlayer :: Player,
  mpContextNextLocation :: Coordinate,
  mpContextLastRoom :: ActiveRoom,
  mpContextNextRoomDefinition :: RoomDefinition
}

data ValidatedCommand = ValidatedMoveCommand MovePlayerContext

processCommand :: PlayerId -> Command -> GameState -> ((Either FailFeedback String), GameState)
processCommand targetPlayerId (Move direction) originalState =
  either handleFailure adjustWorld $ MovePlayerContext <$> maybePlayer <*> maybeNextLocation <*> maybeCurrentRoom <*> maybeNextRoom where
    handleFailure reason = (Left reason, originalState)
    adjustWorld context = mapFst Right $ applyToWorld (ValidatedMoveCommand context) originalState
    maybeCurrentRoom = (flip activeRoomForCommand) originalState =<< maybePlayerLocation
    maybeNextRoom = (flip roomDefinitionForCommand) originalState =<< maybeNextLocation
    maybeNextLocation = move direction <$> maybePlayerLocation
    maybePlayerLocation = playerLocation <$> maybePlayer
    maybePlayer = playerForCommand targetPlayerId originalState

processCommand targetPlayerId LookAtCurrentRoom originalState =
  either handleFailure roomDesc maybeCurrentRoom where
    handleFailure reason = ((Left reason), originalState)
    roomDesc room = ((Right (roomDescription room)), originalState)
    maybeCurrentRoom = (flip roomDefinitionForCommand) originalState =<< maybePlayerLocation
    maybePlayerLocation = playerLocation <$> maybePlayer
    maybePlayer = playerForCommand targetPlayerId originalState

processCommand targetPlayerId (Look direction) originalState =
  either handleFailure roomDesc maybeRoom where
    handleFailure reason = ((Left reason), originalState)
    roomDesc room = ((Right (roomDescription room)), originalState)
    maybeRoom = (flip roomDefinitionForCommand) originalState =<< maybeNextLocation
    maybeNextLocation = move direction <$> maybePlayerLocation
    maybePlayerLocation = playerLocation <$> maybePlayer
    maybePlayer = playerForCommand targetPlayerId originalState

activeRoomForCommand :: Coordinate -> GameState -> Either FailFeedback ActiveRoom
activeRoomForCommand coord state = maybe (Left (NoSuchActiveRoom coord)) Right $ Map.lookup coord (activeRooms state)

roomDefinitionForCommand :: Coordinate -> GameState -> Either FailFeedback RoomDefinition
roomDefinitionForCommand coord state = maybe (Left (NoSuchRoom coord)) Right $ findRoomDefinition coord state

playerForCommand :: PlayerId -> GameState -> Either FailFeedback Player
playerForCommand player state = maybe (Left $ NoSuchPlayer player) Right $ Map.lookup player $ activePlayers state

findRoomDefinition :: Coordinate -> GameState -> Maybe RoomDefinition
findRoomDefinition coordinate state = Map.lookup coordinate (worldRooms (gameWorld state))

applyToWorld :: ValidatedCommand -> GameState -> (String, GameState)
applyToWorld (ValidatedMoveCommand context) originalState =
  result where
    result = ((roomDescription (mpContextNextRoomDefinition context)), newState)
    newState = stateWithActiveRoom {
      activePlayers = newActivePlayers,
      activeRooms = newActiveRooms
    }
    stateWithActiveRoom = (snd ensureRoomActiveResult)
    nextActiveRoom = (fst ensureRoomActiveResult)
    ensureRoomActiveResult = ensureRoomActive (mpContextNextRoomDefinition context) originalState
    newActivePlayers = Map.insert (playerId originalPlayer) newPlayer $ activePlayers originalState
    newPlayer = originalPlayer { playerLocation = (mpContextNextLocation context) }
    originalPlayer = (mpContextPlayer context)
    newActiveRooms = updateLastRoom $ updateNewRoom (activeRooms originalState)
    updateNewRoom = Map.insert (activeRoomCoordinate nextActiveRoom) (addPlayerToRoom (playerId originalPlayer) nextActiveRoom)
    updateLastRoom = Map.insert (activeRoomCoordinate (mpContextLastRoom context)) (removePlayerFromRoom (playerId originalPlayer) (mpContextLastRoom context))

sendCommand :: RunningWorld -> PlayerId -> Command -> IO (Either FailFeedback String)
sendCommand world targetPlayerId command = withStateAndResult world $ RunningWorld.processCommand targetPlayerId command
