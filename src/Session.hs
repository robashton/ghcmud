module Session (
  sessionStart,
  processCommand,
  Command(..),
  Session(..)
) where

import WorldDefinition
import Control.Applicative

data Session = Session {
  sessionPlayer :: Player,
  sessionRoom :: RoomDefinition
} deriving (Show)

data Command = Move Direction
             | Look Direction
             | LookAtCurrentRoom
   deriving (Show, Eq)

sessionStart :: Coordinate -> Player -> WorldDefinition -> Either FailFeedback Session
sessionStart xy player world =
  createSession <$> findRoom xy world
    where createSession room = Session {
      sessionPlayer = player,
      sessionRoom = room
      }

processCommand :: Command -> WorldDefinition -> Session -> Either FailFeedback (String, Session)
processCommand (Move direction) world session =
   either Left updateSessionRoom $ findRoom newPosition world where
     room = sessionRoom session
     currentPosition = roomId room
     newPosition = move direction currentPosition
     updateSessionRoom newRoom = Right (roomDescription newRoom, session { sessionRoom = newRoom })

processCommand (Look direction) world session =
  either noRoomRound describeRoom $ findRoom newPosition world where
    room = sessionRoom session
    currentPosition = roomId room
    newPosition = move direction currentPosition
    describeRoom otherRoom= Right (roomDescription otherRoom, session)
    noRoomRound _ = Right ("There is nothing there", session)

processCommand LookAtCurrentRoom world session@(Session { sessionRoom = room}) =
  Right (roomDescription room, session)
