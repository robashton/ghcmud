module World (
  move,
  findRoom,
  Player(..),
  Monster(..),
  Room(..),
  World(..),
  Direction(..),
  Coordinate(..),
  FailFeedback(..)
) where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

data Direction = West | North | East | South
  deriving (Show, Eq)

data FailFeedback = RoomDoesNotExist

data Coordinate = Coordinate Integer Integer
  deriving (Show, Eq, Ord)

data Player = Player {
  playerHealth :: Integer,
  playerLevel :: Integer,
  playerExperience :: Integer
  } deriving (Show)

data Monster = Monster {
  monsterName :: String,
  monsterLevel :: Integer,
  monsterHealth :: Integer
} deriving (Show)

data Room = Room {
  roomId :: Coordinate,
  roomDescription :: String,
  monsters :: [Monster]
} deriving (Show)

data World = World {
  worldRooms :: Map.Map Coordinate Room
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

findRoom :: Coordinate -> World -> Either FailFeedback Room
findRoom xy world = maybe (Left RoomDoesNotExist) (Right) $ Map.lookup xy $ worldRooms world

addRoom :: World -> Room -> World
addRoom world@(World { worldRooms = existingRooms }) room =
  world { worldRooms = (Map.insert (roomId room) room existingRooms) }

