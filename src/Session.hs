module Session where

import World
import Control.Applicative

data Session = Session {
  sessionPlayer :: Player,
  sessionRoom :: Room
} deriving (Show)

data Command = Move Direction
             | Look Direction

sessionStart :: Coordinate -> Player -> World -> Either FailFeedback Session
sessionStart xy player world =
  createSession <$> findRoom xy world
    where createSession room = Session {
      sessionPlayer = player,
      sessionRoom = room
      }

processCommand :: Command -> World -> Session -> Either FailFeedback Session
processCommand (Move direction) world session =
  let room = sessionRoom session
      currentPosition = roomId room
      newPosition = move direction currentPosition in
   case findRoom newPosition world of
     Left err -> Left err
     Right newRoom -> Right session { sessionRoom = newRoom }

processCommand (Look direction) world session =
  let room = sessionRoom session
      currentPosition = roomId room
      newPosition = move direction currentPosition in
   case findRoom newPosition world of
     Left err -> Left err
     Right newRoom -> Right session { sessionRoom = newRoom }
