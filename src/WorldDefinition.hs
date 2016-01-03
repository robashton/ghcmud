module WorldDefinition (
  move,
  findRoom,
  PlayerId(..),
  Player(..),
  Monster(..),
  RoomDefinition(..),
  WorldDefinition(..),
  Direction(..),
  Coordinate(..),
  FailFeedback(..)
) where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

data Direction = West | North | East | South
  deriving (Show, Eq)

data FailFeedback = RoomDoesNotExist
newtype PlayerId = PlayerId String
  deriving (Show, Ord, Eq)

data Coordinate = Coordinate Integer Integer
  deriving (Show, Eq, Ord)

data Player = Player {
  playerId :: PlayerId,
  playerHealth :: Integer,
  playerLevel :: Integer,
  playerExperience :: Integer,
  playerLocation :: Coordinate
  } deriving (Show)

data Monster = Monster {
  monsterName :: String,
  monsterLevel :: Integer,
  monsterHealth :: Integer
} deriving (Show)

data RoomDefinition = RoomDefinition {
  roomId :: Coordinate,
  roomDescription :: String,
  monsters :: [Monster]
} deriving (Show)

data WorldDefinition = WorldDefinition {
  worldRooms :: Map.Map Coordinate RoomDefinition
} deriving (Show)

move :: Direction -> Coordinate -> Coordinate
move West = west
move East = east
move North = north
move South = south

south :: Coordinate -> Coordinate
south (Coordinate x y) = Coordinate x (y+1)

north :: Coordinate -> Coordinate
north (Coordinate x y) = Coordinate x (y-1)

west :: Coordinate -> Coordinate
west (Coordinate x y) = Coordinate (x-1) y

east :: Coordinate -> Coordinate
east (Coordinate x y) = Coordinate (x+1) y

findRoom :: Coordinate -> WorldDefinition -> Either FailFeedback RoomDefinition
findRoom xy world = maybe (Left RoomDoesNotExist) (Right) $ Map.lookup xy $ worldRooms world
