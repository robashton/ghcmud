module Gaia where

import World
import qualified Data.Map as Map

build :: World
build = World {
  worldRooms = Map.fromList $ gaiaRooms
}

gaiaRooms :: [(Coordinate, Room)]
gaiaRooms = [ ]


