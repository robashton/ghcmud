{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty

import Data.Text.Lazy(pack,unpack)

import WorldDefinitionLoading
import WorldDefinition
import CommandParsing

import WorldInstance
import RunningWorld

import Control.Applicative ((<*>), empty)
import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mconcat)
import Control.Concurrent

import Network.Wai.Middleware.Static

startFailure :: FailFeedback -> IO()
startFailure RoomDoesNotExist = print ("There is no room to start in")

loadFailure :: WorldLoadFailure -> IO()
loadFailure (RoomParseFailure filename) = print ("Failed to parse room: " ++ filename)
loadFailure (RoomsFailedToLoad fails) = print ("Failed to load rooms: " ++ (show fails))
loadFailure NoRoomsInWorld = print "No rooms in the loaded world"
loadFailure OrphanedRoomsFound = print "There were rooms in the world that weren't linked"
loadFailure (RoomNotFound room) = print ("Room " ++ room ++ " not found in world")

main :: IO ()
main = either loadFailure startGame =<< loadDir "gaia/world"

startGame :: WorldDefinition -> IO()
startGame definition = do
  world <- createRunningWorld definition
  webserver world

webserver :: RunningWorld -> IO ()
webserver world = scotty 3000 $ do
  get "/" $ file "www/index.html"
  get "/login/:user" $ handleLogin world
  post "/command/:user" $ handleCommand world
  middleware $ staticPolicy (noDots >-> addBase "www")

-- TODO: Code
genericShitRequest :: String -> ActionM ()
genericShitRequest msg = do 
  html $ pack msg

handleLogin :: RunningWorld -> ActionM()
handleLogin world = do
  user <- param "user"
  addPlayerResult <- liftIO $ addPlayerToWorld (PlayerId $ unpack user) world
  either addFailure addSuccess addPlayerResult where
    addFailure _ = genericShitRequest "Bollocks user"
    addSuccess _ = html $ "Player created"

handleCommand :: RunningWorld -> ActionM()
handleCommand world = do
  command <- param "command"
  either failParsedCommand sendParsedCommand $ parseCommand command where
    failParsedCommand reason = genericShitRequest $ mconcat ["No parsy command: ", (show reason)]
    sendParsedCommand cmd = do
      user <- param "user" 
      commandResult <- liftIO $ sendCommand world (PlayerId $ unpack user) cmd
      either commandFailure commandSuccess commandResult
    commandFailure why = genericShitRequest $ show why
    commandSuccess result = html $ pack result


addPlayerToWorld :: PlayerId -> RunningWorld -> IO (Either InstanceFailure GenericSuccess)
addPlayerToWorld newPlayerId world =
  addPlayer world awesomePlayer where
    awesomePlayer = Player {
      playerId = newPlayerId,
      playerHealth = 100,
      playerLevel = 999,
      playerExperience = 5,
      playerLocation = Coordinate 0 0
      }

defaultPlayerId :: PlayerId
defaultPlayerId = PlayerId "bob"

startInputLoop :: RunningWorld -> IO()
startInputLoop game = (handleCommandResult game) =<< sendCommand game defaultPlayerId LookAtCurrentRoom

handleCommandResult :: RunningWorld -> (Either InstanceFailure String) -> IO()
handleCommandResult game (Right result) = print result >> inputLoop game
handleCommandResult game (Left result) = print (show result) >> inputLoop game

inputLoop :: RunningWorld -> IO()
inputLoop game  =
  do
    instruction <- getLine
    either failParse actionCommand (parseCommand instruction) where
      failParse _ = print "No idea what you're saying here" >> inputLoop game
      actionCommand command = (handleCommandResult game) =<< (sendCommand game defaultPlayerId command)
