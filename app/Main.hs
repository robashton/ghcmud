module Main where

import Loader
import WorldDefinition
import Session
import Parser
import RunningWorld

import Control.Applicative ((<*>), empty)
import Control.Monad (liftM)
import Control.Concurrent

startFailure :: FailFeedback -> IO()
startFailure RoomDoesNotExist = print ("There is no room to start in")

loadFailure :: WorldLoadFailure -> IO()
loadFailure (RoomParseFailure filename) = print ("Failed to parse room: " ++ filename)
loadFailure (RoomsFailedToLoad fails) = print ("Failed to load rooms: " ++ (show fails))
loadFailure NoRoomsInWorld = print "No rooms in the loaded world"
loadFailure OrphanedRoomsFound = print "There were rooms in the world that weren't linked"
loadFailure (RoomNotFound room) = print ("Room " ++ room ++ " not found in world")

main :: IO ()
main = either loadFailure startGame =<< loadDir "gaia/world"

startGame :: WorldDefinition -> IO()
startGame definition = do
  world <- createRunningWorld definition
  addPlayerToWorld world
  startInputLoop world

addPlayerToWorld :: RunningWorld -> IO PlayerId
addPlayerToWorld game =
  let awesomePlayer = Player {
      playerId = defaultPlayerId,
      playerHealth = 100,
      playerLevel = 999,
      playerExperience = 5,
      playerLocation = Coordinate 0 0
       } in
    do
      addPlayerResult <- addPlayer game awesomePlayer
      print (show addPlayerResult)
      return (playerId awesomePlayer)

defaultPlayerId :: PlayerId
defaultPlayerId = PlayerId "bob"

startInputLoop :: RunningWorld -> IO()
startInputLoop game = (handleCommandResult game) =<< sendCommand game defaultPlayerId LookAtCurrentRoom

handleCommandResult :: RunningWorld -> (Either FailFeedback String) -> IO()
handleCommandResult game (Right result) = print result >> inputLoop game
handleCommandResult game (Left result) = print (show result) >> inputLoop game

inputLoop :: RunningWorld -> IO()
inputLoop game  =
  do
    instruction <- getLine
    either failParse actionCommand (parseCommand instruction) where
      failParse _ = print "No idea what you're saying here" >> inputLoop game
      actionCommand command = (handleCommandResult game) =<< (sendCommand game defaultPlayerId command)
