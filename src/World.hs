module World where

import qualified Data.Map as Map

data Direction = West | North | East | South
  deriving (Show)

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

buildWorld :: World
buildWorld = World {
  worldRooms = Map.fromList []
}

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
findRoom xy world = case Map.lookup xy $ worldRooms world of
                      Nothing -> Left RoomDoesNotExist
                      Just room -> Right room

addRoom :: World -> Room -> World
addRoom world@(World { worldRooms = existingRooms }) room =
  world { worldRooms = (Map.insert (roomId room) room existingRooms) }





