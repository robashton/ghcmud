module Parser where

import World
import Session
import Control.Applicative


import Data.Char (toLower)

data ParseFailure = ShiteCommand
                  | ParseFailure String
                  deriving (Show)

parseCommand :: String -> Either ParseFailure Command
parseCommand = readCommand . stripFillerWords . words . (map toLower)

stripFillerWords :: [String] -> [String]
stripFillerWords = filter (`notElem` ["the", "to"])

isMoveCommand :: String -> Bool
isMoveCommand "move" = True
isMoveCommand "go" = True
isMoveCommand "walk" = True
isMoveCommand _ = False

isLookCommand :: String -> Bool
isLookCommand "look" = True
isLookCommand "peer" = True
isLookCommand "scan" = True
isLookCommand _ = False

readCommand :: [String] -> Either ParseFailure Command
readCommand (cmd:xs)
  | isMoveCommand(cmd) = Move <$> parseDirection xs
  | isLookCommand(cmd) = case xs of
                           [] -> Right LookAtCurrentRoom
                           _ -> Look <$> parseDirection xs
  | otherwise = Left ShiteCommand

parseDirection :: [String] -> Either ParseFailure Direction
parseDirection ("west":_) = Right West
parseDirection ("east":_) = Right East
parseDirection ("north":_) = Right North
parseDirection ("south":_) = Right South
parseDirection _ = Left $ ParseFailure "unrecognised direction"


