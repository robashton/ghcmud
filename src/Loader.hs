{-# LANGUAGE DeriveGeneric #-}

module Loader (
--  loadDir
) where

import World
import System.FilePath
import GHC.Generics
import System.Directory
import qualified Data.ByteString.Lazy as B
import Data.Aeson (FromJSON, ToJSON, decode, encode)

data RawDirection = North | West | South | East
  deriving (Show, Generic)

data RawRoom = RawRoom {
  description :: String,
  directions :: [ ( RawDirection, String )]
} deriving (Show, Generic)


data WorldLoadFailure = WorldLoadFailure String

instance FromJSON RawRoom
instance FromJSON RawDirection

loadDir :: FilePath -> IO (Either WorldLoadFailure World)
loadDir dir = createWorld <$> (parseRooms =<< (normaliseRoomPaths dir <$> getDirectoryContents dir))

createWorld :: (Either WorldLoadFailure [RawRoom]) -> Either WorldLoadFailure World
createWorld (Left msg) = Left msg
createWorld _ = Right (buildWorld)

normaliseRoomPaths :: FilePath -> [FilePath] -> [FilePath]
normaliseRoomPaths rootDir rooms = map joinPaths rooms where
  joinPaths room = joinPath [ rootDir, room ]

parseRooms :: [FilePath] -> IO (Either WorldLoadFailure [RawRoom])
parseRooms paths = sequence <$> (sequence $ map loadRoom paths)

loadRoom :: FilePath -> IO (Either WorldLoadFailure RawRoom)
loadRoom file = (transformRoomLoad file) <$> decode <$> B.readFile file

transformRoomLoad :: FilePath -> Maybe RawRoom -> Either WorldLoadFailure RawRoom
transformRoomLoad _ (Just room) = Right room
transformRoomLoad filepath Nothing = Left (WorldLoadFailure ("Failed to load room: " ++ filepath))
