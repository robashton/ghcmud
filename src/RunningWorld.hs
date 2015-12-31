module RunningWorld where

import World
import Parser
import Session

import Control.Concurrent
import Control.Monad

data GameState = GameState {
  gameWorld :: World,
  gameSession :: Session
  } deriving (Show)

createRunningWorld :: World -> Either FailFeedback (IO (MVar GameState))
createRunningWorld world = newMVar <$> (GameState world) <$> sessionStart (Coordinate 0 0) createPlayer world

sendCommand :: (MVar GameState) -> Command -> IO String
sendCommand m c = do
                    (result, newState) <- ((handleCommand c) <$> takeMVar m)
                    putMVar m newState
                    return result

handleCommand :: Command -> GameState -> (String, GameState)
handleCommand command state@( GameState { gameSession = session, gameWorld = world }) =
  case processCommand command world session of
    Left err -> (translateCommandError err, state)
    Right (feedback, newSession) -> (feedback, state { gameSession = newSession })

translateCommandError :: FailFeedback -> String
translateCommandError RoomDoesNotExist = "There is no room there, doofus"

createPlayer :: Player
createPlayer =
  Player {
    playerHealth = 10,
    playerLevel = 1,
    playerExperience = 0
  }

