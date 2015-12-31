module Main where

import Loader
import World
import Session
import Parser


import Control.Applicative ((<*>), empty)
import Control.Monad (liftM)

data GameState = GameState {
  gameWorld :: World,
  gameSession :: Session
  } deriving (Show)

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
  case initGameState world of
    Left RoomDoesNotExist -> errorOut "The room you were meant to start in did not exist"
    Right state ->
      print "Your game starts now" >> startInputLoop state

errorOut :: String -> IO()
errorOut msg = print ("Arse, something went tits up, here is a msg about that: " ++ msg)

translateCommandError :: FailFeedback -> String
translateCommandError RoomDoesNotExist = "There is no room there, doofus"

handleEndReason :: String -> IO()
handleEndReason = print

handleTextInstruction :: GameState -> String -> Either String (String, GameState)
handleTextInstruction state@(GameState { gameSession = session, gameWorld = world }) command =
  case parseCommand command of
    Left _ -> Right ("No idea what you're trying to say here", state)
    Right instruction -> case processCommand instruction world session of
                           Left err -> Right (translateCommandError err, state)
                           Right (feedback, newSession) -> Right (feedback, state { gameSession = newSession })

describeCurrentRoom :: Session -> String
describeCurrentRoom Session { sessionRoom = Room { roomDescription = desc } } = desc


startInputLoop :: GameState -> IO()
startInputLoop state@( GameState { gameSession = session}) =
  do
    print $ describeCurrentRoom session
    inputLoop state

inputLoop :: GameState -> IO()
inputLoop state =
  do
    instruction <- getLine
    case handleTextInstruction state instruction of
      Left endReason -> handleEndReason endReason
      Right (msg, newState) ->
          print msg >> inputLoop newState

initGameState :: World -> Either FailFeedback GameState
initGameState world =
  (GameState world) <$> sessionStart (Coordinate 0 0) createPlayer world

createPlayer :: Player
createPlayer =
  Player {
    playerHealth = 10,
    playerLevel = 1,
    playerExperience = 0
  }
