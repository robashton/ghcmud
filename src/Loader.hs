{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Loader (
  loadDir,
  WorldLoadFailure(WorldLoadFailure)
) where

import World

import Safe
import System.FilePath
import GHC.Generics
import System.Directory
import Control.Monad
import Data.Aeson (FromJSON, ToJSON, decode, encode)

import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map

data RawDirection = North | West | South | East
  deriving (Ord, Eq, Show, Generic)

data RawRoom = RawRoom {
  rawRoomId :: String,
  description :: String,
  north :: Maybe String,
  west :: Maybe String,
  east :: Maybe String,
  south :: Maybe String
} deriving (Show, Generic)

data WorldLoadFailure = WorldLoadFailure String

instance FromJSON RawRoom
instance FromJSON RawDirection

loadDir :: FilePath -> IO (Either WorldLoadFailure World)
loadDir dir = liftM (>>= createWorld) $ (fullRoomPaths dir) >>= parseRooms -- with apologies to @bitemyapp

fullRoomPaths :: FilePath -> IO [FilePath]
fullRoomPaths dir = (normaliseRoomPaths dir <$> filter (`notElem` [".", ".."]) <$> getDirectoryContents dir)

normaliseRoomPaths :: FilePath -> [FilePath] -> [FilePath]
normaliseRoomPaths rootDir rooms = map joinPaths rooms where
                                       joinPaths room = joinPath [ rootDir, room ]

createWorld :: (Map.Map String RawRoom) -> Either WorldLoadFailure World
createWorld input = worldFromRooms <$> convertRooms input

worldFromRooms :: (Map.Map Coordinate Room) -> World
worldFromRooms _ = buildWorld

convertRooms :: (Map.Map String RawRoom) -> Either WorldLoadFailure (Map.Map Coordinate Room)
convertRooms rooms = case headMay $ Map.toList rooms of
                       Nothing -> Left (WorldLoadFailure "There are no rooms defined in this world")
                       Just (firstId, _firstRoom) ->
                        finaliseConversion =<< processRoom (Coordinate 0 0) firstId (BuilderState ((Map.fromList []), rooms))

finaliseConversion :: BuilderState -> Either WorldLoadFailure (Map.Map Coordinate Room)
finaliseConversion (BuilderState (output, input))
  | Map.null input = Right output
  | otherwise = Left (WorldLoadFailure "Rooms left after traversing world!")

newtype BuilderState = BuilderState ((Map.Map Coordinate Room), (Map.Map String RawRoom))

processRoom :: Coordinate -> String -> BuilderState -> Either WorldLoadFailure BuilderState
processRoom coord roomId (BuilderState (output, input)) =
  case Map.lookup roomId input of
    Nothing -> Left (WorldLoadFailure ("Couldn't find " ++ roomId ++ " in world"))
    Just room ->
      let inputWithoutRoom = Map.delete roomId input
          outputWithRoom = Map.insert coord (convertRoom room coord) output in
          (processSiblingRoom room coord Loader.West) =<<
          (processSiblingRoom room coord Loader.East) =<<
          (processSiblingRoom room coord Loader.South) =<<
          processSiblingRoom room coord Loader.North (BuilderState (outputWithRoom, inputWithoutRoom))


processSiblingRoom :: RawRoom -> Coordinate -> RawDirection -> BuilderState -> Either WorldLoadFailure BuilderState
processSiblingRoom currentRoom currentCoordinate direction (BuilderState (output, input)) =
  let fn = case direction of
            Loader.West -> Loader.west
            Loader.East -> Loader.east
            Loader.South -> Loader.south
            Loader.North -> Loader.north in
              case (fn currentRoom) of
                Nothing -> Right $ BuilderState (output, input)
                Just siblingRoomName -> processRoom (adjust currentCoordinate direction) siblingRoomName $ BuilderState (output, input)

adjust :: Coordinate -> RawDirection -> Coordinate
adjust (Coordinate x y) direction
  | direction == Loader.West = (Coordinate (x-1) y)
  | direction == Loader.East = (Coordinate (x+1) y)
  | direction == Loader.South = (Coordinate x (y+1))
  | direction == Loader.North = (Coordinate x (y-1))

convertRoom :: RawRoom -> Coordinate -> Room
convertRoom RawRoom { description = desc } coord =
  Room {
    roomId = coord,
    roomDescription = desc,
    monsters = [] }

parseRooms :: [FilePath] -> IO (Either WorldLoadFailure (Map.Map String RawRoom))
parseRooms paths = (fmap roomsIntoMap) <$> sequence <$> (sequence $ map loadRoom paths)

roomsIntoMap :: [RawRoom] -> (Map.Map String RawRoom)
roomsIntoMap rooms = Map.fromList $ map intoPair rooms
                                    where intoPair room@(RawRoom { rawRoomId = roomId }) = ( roomId, room )

loadRoom :: FilePath -> IO (Either WorldLoadFailure RawRoom)
loadRoom file = (maybeToEither file) <$> decode <$> B.readFile file

maybeToEither :: FilePath -> Maybe RawRoom -> Either WorldLoadFailure RawRoom
maybeToEither _ (Just room) = Right room
maybeToEither filepath Nothing = Left (WorldLoadFailure ("Failed to load room: " ++ filepath))
