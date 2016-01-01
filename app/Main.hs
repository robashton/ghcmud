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
startGame = either startFailure ((=<<) startInputLoop) . createRunningWorld

describeCurrentRoom :: Session -> String
describeCurrentRoom Session { sessionRoom = RoomDefinition { roomDescription = desc } } = desc

startInputLoop :: RunningWorld -> IO()
startInputLoop game = (handleCommandResult game) =<< sendCommand game LookAtCurrentRoom

handleCommandResult :: RunningWorld -> String -> IO()
handleCommandResult game result = print result >> inputLoop game

inputLoop :: RunningWorld -> IO()
inputLoop game =
  do
    instruction <- getLine
    either failParse actionCommand (parseCommand instruction) where
      failParse _ = print "No idea what you're saying here" >> inputLoop game
      actionCommand command = (handleCommandResult game) =<< (sendCommand game command)
