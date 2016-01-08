module WorldInstance
(
  PlayerId(..),
  Player(..),
  ActiveRoom(..),
  WorldInstance(..),
  GenericSuccess(..),
  InstanceFailure(..),
  addPlayerToGame,
  processCommand
) where

import WorldDefinition

import Control.Monad
import Data.Either
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Graph.Inductive.Query.Monad (mapFst)

data ActiveRoom = ActiveRoom {
  activeRoomCoordinate :: Coordinate,
  activeRoomPlayers :: Set.Set PlayerId
} deriving (Show)

data WorldInstance = WorldInstance {
  gameWorld :: WorldDefinition,
  activeRooms :: Map.Map Coordinate ActiveRoom,
  activePlayers :: Map.Map PlayerId Player
  } deriving (Show)

data MovePlayerContext = MovePlayerContext {
  mpContextPlayer :: Player,
  mpContextNextLocation :: Coordinate,
  mpContextLastRoom :: ActiveRoom,
  mpContextNextRoomDefinition :: RoomDefinition
} deriving (Show)

newtype PlayerId = PlayerId String
  deriving (Show, Ord, Eq)

data Player = Player {
  playerId :: PlayerId,
  playerHealth :: Integer,
  playerLevel :: Integer,
  playerExperience :: Integer,
  playerLocation :: Coordinate
  } deriving (Show)

data InstanceFailure = NoSuchActiveRoom Coordinate
                     | NoSuchRoomDefinition Coordinate
                     | NoSuchPlayer PlayerId
                     deriving (Show)

data ValidatedCommand = ValidatedMoveCommand MovePlayerContext
  deriving (Show)

data GenericSuccess = GenericSuccess 
  deriving (Show)

addPlayerToGame :: Player -> WorldInstance -> (Either InstanceFailure GenericSuccess, WorldInstance)
addPlayerToGame player initialState =
  either failure success $ playerIntoWorld player =<< (maybeEnsureRoomActive (playerLocation player) initialState) where
    failure reason = (Left reason, initialState)
    success s = (Right GenericSuccess, stateWithPlayer s)
    stateWithPlayer s = s { activePlayers = Map.insert (playerId player) player (activePlayers s) }

processCommand :: PlayerId -> Command -> WorldInstance -> ((Either InstanceFailure String), WorldInstance)
processCommand targetPlayerId (Move direction) originalState =
  either failure adjustWorld $ MovePlayerContext <$> maybePlayer <*> maybeNextLocation <*> maybeCurrentRoom <*> maybeNextRoom where
    failure reason = (Left reason, originalState)
    adjustWorld context = mapFst Right $ applyCommandToWorld (ValidatedMoveCommand context) originalState
    maybeCurrentRoom = (flip activeRoomForCommand) originalState =<< maybePlayerLocation
    maybeNextRoom = (flip roomDefinitionForCommand) originalState =<< maybeNextLocation
    maybeNextLocation = move direction <$> maybePlayerLocation
    maybePlayerLocation = playerLocation <$> maybePlayer
    maybePlayer = playerForCommand targetPlayerId originalState

processCommand targetPlayerId LookAtCurrentRoom originalState =
  either failure roomDesc maybeCurrentRoom where
    failure reason = ((Left reason), originalState)
    roomDesc room = ((Right (roomDescription room)), originalState)
    maybeCurrentRoom = (flip roomDefinitionForCommand) originalState =<< maybePlayerLocation
    maybePlayerLocation = playerLocation <$> maybePlayer
    maybePlayer = playerForCommand targetPlayerId originalState

processCommand targetPlayerId (Look direction) originalState =
  either failure roomDesc maybeRoom where
    failure reason = ((Left reason), originalState)
    roomDesc room = ((Right (roomDescription room)), originalState)
    maybeRoom = (flip roomDefinitionForCommand) originalState =<< maybeNextLocation
    maybeNextLocation = move direction <$> maybePlayerLocation
    maybePlayerLocation = playerLocation <$> maybePlayer
    maybePlayer = playerForCommand targetPlayerId originalState

maybeEnsureRoomActive :: Coordinate -> WorldInstance -> Either InstanceFailure WorldInstance
maybeEnsureRoomActive coordinate originalState =
  maybe (Left $ NoSuchRoomDefinition coordinate) setupRoom $ findRoomDefinition coordinate originalState where
    setupRoom definition = Right $ snd $ ensureRoomActive definition originalState

ensureRoomActive :: RoomDefinition -> WorldInstance -> (ActiveRoom, WorldInstance)
ensureRoomActive room originalState =
  maybe makeRoomActive roomAlreadyActive $ Map.lookup coordinate currentActiveRooms where
    roomAlreadyActive room = (room, originalState)
    makeRoomActive = (newActiveRoom, originalState { activeRooms = Map.insert coordinate newActiveRoom currentActiveRooms })
    coordinate = (roomId room)
    currentActiveRooms = (activeRooms originalState)
    newActiveRoom = ActiveRoom coordinate mempty

playerIntoWorld :: Player -> WorldInstance -> Either InstanceFailure WorldInstance
playerIntoWorld player originalState =
  maybe failure success $ Map.lookup coordinate currentActiveRooms where
    failure = Left $ NoSuchRoomDefinition coordinate
    success room = Right originalState { activeRooms = Map.insert coordinate (addPlayerToRoom (playerId player) room) currentActiveRooms }
    currentActiveRooms = (activeRooms originalState)
    coordinate = (playerLocation player)

addPlayerToRoom :: PlayerId -> ActiveRoom -> ActiveRoom
addPlayerToRoom targetPlayerId room =
  room { activeRoomPlayers = Set.insert targetPlayerId (activeRoomPlayers room) }

removePlayerFromRoom :: PlayerId -> ActiveRoom -> ActiveRoom
removePlayerFromRoom targetPlayerId room =
  room { activeRoomPlayers = Set.delete targetPlayerId (activeRoomPlayers room) }

applyCommandToWorld :: ValidatedCommand -> WorldInstance -> (String, WorldInstance)
applyCommandToWorld (ValidatedMoveCommand context) originalState =
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

activeRoomForCommand :: Coordinate -> WorldInstance -> Either InstanceFailure ActiveRoom
activeRoomForCommand coord state = maybe (Left $ NoSuchActiveRoom coord) Right $ Map.lookup coord (activeRooms state)

roomDefinitionForCommand :: Coordinate -> WorldInstance -> Either InstanceFailure RoomDefinition
roomDefinitionForCommand coord state = maybe (Left $ NoSuchRoomDefinition coord) Right $ findRoomDefinition coord state

playerForCommand :: PlayerId -> WorldInstance -> Either InstanceFailure Player
playerForCommand player state = maybe (Left $ NoSuchPlayer player) Right $ Map.lookup player $ activePlayers state

findRoomDefinition :: Coordinate -> WorldInstance -> Maybe RoomDefinition
findRoomDefinition coordinate state = Map.lookup coordinate (worldRooms (gameWorld state))
