module Parser where

import World
import Session
import Control.Applicative

data ParseFailure = ParseFailure

parseCommand :: String -> Either ParseFailure Command
parseCommand _ = Right $ Move West
