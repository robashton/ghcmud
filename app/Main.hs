module Main where

import Loader
import World
import Session
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
      Left (WorldLoadFailure msg) -> errorOut msg
      Right world -> startGame world

startGame :: World -> IO()
startGame world =
  case initGameState world of
    Left RoomDoesNotExist -> errorOut "The room you were meant to start in did not exist"
    Right state -> startGameLoop state

errorOut :: String -> IO()
errorOut msg = print ("Arse, something went tits up, here is a msg about that: " ++ msg)

startGameLoop :: GameState -> IO()
startGameLoop state = print "Ok"

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
