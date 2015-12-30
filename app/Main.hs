module Main where

import Loader
import World

main :: IO ()
main = do
  world <- loadDir "gaia/world"
  printThing world

printThing :: Either WorldLoadFailure World -> IO()
printThing (Left (WorldLoadFailure msg)) = print msg
printThing (Right world) = print (show world)
