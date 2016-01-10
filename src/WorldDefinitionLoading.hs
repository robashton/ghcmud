{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards  #-}

-- This module searches a directory for 'rooms' and returns a constructed 'World'
-- with an automatically determined coordinate system based on traversing the 'rooms' in that world

module WorldDefinitionLoading (
  loadDir,
  WorldLoadFailure(..)
) where

import WorldDefinition

import Safe
import System.FilePath
import GHC.Generics
import System.Directory
import Control.Monad
import Data.Either
import Data.Aeson
import Data.Foldable
import Data.Monoid
import Data.Maybe (fromMaybe)

import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict as Map
import Control.Applicative ((<*>), empty)

data RawRoom = RawRoom {
  rawRoomId :: String,
  description :: String,
  north :: Maybe String,
  west :: Maybe String,
  east :: Maybe String,
  south :: Maybe String
} deriving (Show, Generic)

data WorldLoadFailure = RoomParseFailure String
                      | RoomsFailedToLoad [WorldLoadFailure]
                      | NoRoomsInWorld
                      | OrphanedRoomsFound
                      | RoomNotFound String
  deriving (Show)

newtype BuilderState = BuilderState ((Map.Map Coordinate RoomDefinition), (Map.Map String RawRoom))

instance FromJSON RawRoom where
  parseJSON = withObject "rawroom" $
    \obj -> do
      rawRoomId <- obj .:? "rawRoomId" .!= "unknown"
      description <- obj .: "description"
      north <- obj .:? "north"
      east <- obj .:? "east"
      south <- obj .:? "south"
      west <- obj .:? "west"
      return RawRoom {..}

loadDir :: FilePath -> IO (Either WorldLoadFailure WorldDefinition)
loadDir = fmap createWorld . join . fmap loadRooms . fullRoomPaths

createWorld :: ([WorldLoadFailure], [RawRoom]) -> Either WorldLoadFailure WorldDefinition
createWorld = fmap WorldDefinition . join . fmap convertRooms . checkRoomLoadErrors

checkRoomLoadErrors :: ([WorldLoadFailure], [RawRoom]) -> Either WorldLoadFailure [RawRoom]
checkRoomLoadErrors ([], rooms) = Right rooms
checkRoomLoadErrors (errors, _) = Left $ RoomsFailedToLoad errors

convertRooms :: [RawRoom] -> Either WorldLoadFailure (Map.Map Coordinate RoomDefinition)
convertRooms [] = Left NoRoomsInWorld
convertRooms rooms@(RawRoom { rawRoomId = firstId }:_) =
  ensureAllRoomsProcessed =<< processRoom (Coordinate 0 0) firstId (initialBuildState rooms)

initialBuildState :: [RawRoom] -> BuilderState
initialBuildState rooms = BuilderState (mempty, roomsIntoMap rooms)

roomsIntoMap :: [RawRoom] -> (Map.Map String RawRoom)
roomsIntoMap = Map.fromList . map intoPair
  where intoPair room@(RawRoom { rawRoomId = roomId }) = ( roomId, room )

ensureAllRoomsProcessed :: BuilderState -> Either WorldLoadFailure (Map.Map Coordinate RoomDefinition)
ensureAllRoomsProcessed (BuilderState (output, input)) | Map.null input = Right output
ensureAllRoomsProcessed _ = Left OrphanedRoomsFound

processRoom :: Coordinate -> String -> BuilderState -> Either WorldLoadFailure BuilderState
processRoom coord roomId builderState@(BuilderState (output, input)) =
  fromMaybe unreferenced $ nextRooms <$> referencedRoom where
        referencedRoom = Map.lookup roomId input
        locatedRoom = Map.lookup coord output
        notFound = Left $ RoomNotFound roomId
        unchanged _ = Right builderState
        unreferenced = maybe notFound unchanged locatedRoom
        inputWithoutRoom = Map.delete roomId input
        nextRooms room = foldrM (processSiblingRoom room coord) (addRoom room) [South, West, East, North]
        addRoom room = BuilderState (outputWithRoom, inputWithoutRoom) where
          outputWithRoom = Map.insert coord (convertRoom room coord) output

processSiblingRoom :: RawRoom -> Coordinate -> Direction -> BuilderState -> Either WorldLoadFailure BuilderState
processSiblingRoom currentRoom currentCoordinate direction builderState =
  maybe (Right builderState) nextRoom roomName where
    roomName = sibling direction currentRoom
    nextRoom siblingRoomName = processRoom (move direction currentCoordinate) siblingRoomName builderState

sibling :: Direction -> RawRoom -> Maybe String
sibling West = WorldDefinitionLoading.west
sibling East = WorldDefinitionLoading.east
sibling South = WorldDefinitionLoading.south
sibling North = WorldDefinitionLoading.north

convertRoom :: RawRoom -> Coordinate -> RoomDefinition
convertRoom RawRoom { description = roomDescription } roomId =
  let monsters = [] in RoomDefinition{..}

parseRoom :: FilePath -> B.ByteString -> Either WorldLoadFailure RawRoom
parseRoom file = (addRoomId file) . decode

addRoomId :: FilePath -> Maybe RawRoom -> Either WorldLoadFailure RawRoom
addRoomId filepath = maybe fail addRoom
  where addRoom room = Right room { rawRoomId = dropExtension $ takeBaseName filepath }
        fail = Left $ RoomParseFailure filepath
--
-- IO
--

loadRooms :: [FilePath] -> IO ([WorldLoadFailure], [RawRoom])
loadRooms = fmap partitionEithers . sequence . map loadRoom

loadRoom :: FilePath -> IO (Either WorldLoadFailure RawRoom)
loadRoom file = fmap (parseRoom file) $ (B.readFile file)

fullRoomPaths :: FilePath -> IO [FilePath]
fullRoomPaths dir = fmap (normaliseRoomPaths dir) $ getRooms dir

getRooms :: FilePath -> IO [FilePath]
getRooms = fmap (filter (`notElem` [".", ".."])) . getDirectoryContents

normaliseRoomPaths :: FilePath -> [FilePath] -> [FilePath]
normaliseRoomPaths rootDir = map joinPaths where joinPaths room = joinPath [ rootDir, room ]
