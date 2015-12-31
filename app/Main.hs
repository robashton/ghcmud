module Main where

import Loader
import World
import Session
import Parser
import RunningWorld

import Control.Applicative ((<*>), empty)
import Control.Monad (liftM)
import Control.Concurrent

errorOut :: String -> IO()
errorOut msg = print ("Arse, something went tits up, here is a msg about that: " ++ msg)

main :: IO ()
main =
  do
    loadResult <- loadDir "gaia/world"
    case loadResult of
      Left (RoomParseFailure filename) -> errorOut ("Failed to parse room: " ++ filename)
      Left (RoomsFailedToLoad fails) -> errorOut ("Failed to load rooms: " ++ (show fails))
      Left NoRoomsInWorld -> errorOut "No rooms in the loaded world"
      Left OrphanedRoomsFound -> errorOut "There were rooms in the world that weren't linked"
      Left (RoomNotFound room) -> errorOut ("Room " ++ room ++ " not found in world")
      Right world -> startGame world

startGame :: World -> IO()
startGame world =
  case createRunningWorld world of
    Left RoomDoesNotExist -> errorOut "The room you were meant to start in did not exist"
    Right game ->
      do
        print "Your game starts now"
        game >>= startInputLoop

handleEndReason :: String -> IO()
handleEndReason = print

describeCurrentRoom :: Session -> String
describeCurrentRoom Session { sessionRoom = Room { roomDescription = desc } } = desc

startInputLoop :: RunningWorld -> IO()
startInputLoop game =
  do
    --print $ describeCurrentRoom session
    inputLoop game

inputLoop :: RunningWorld -> IO()
inputLoop game =
  do
    instruction <- getLine
    case parseCommand instruction of
      Left _nope -> print "No idea what you're saying here" >> inputLoop game
      Right command ->
        do
          result <- sendCommand game command
          print result
          inputLoop game



