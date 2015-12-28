module Parser where

import Control.Applicative

data Direction = West | North | East | South
  deriving Show

data Instruction = Move Direction

readInstruction :: IO Instruction
readInstruction = parseInstruction <$> getLine

parseInstruction :: String -> Instruction
parseInstruction = switchInstruction <$> nextWord . words

nextWord :: [String] -> Maybe (String, [String])
nextWord [] = Nothing
nextWord (x:xs) = if isFillerWord x
                     then nextWord xs
                     else Just (x, xs)

fillerWords :: [String]
fillerWords = [ "the" ]

isFillerWord :: String -> Bool
isFillerWord x = elem x fillerWords
