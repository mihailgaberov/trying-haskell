{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Data.Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID
import Data.UUID.V4
import Game
import Network.HTTP.Types.Status (status404)
import System.Environment (lookupEnv)
import System.Random (randomRIO)
import Web.Scotty

-- Simple game store: maps game IDs to game states
type GameId = Text

type GameStore = MVar (Map GameId GameState)

-- JSON instances for GameState
instance ToJSON GameState where
  toJSON NotStarted = object ["status" .= ("notStarted" :: Text)]
  toJSON (Playing s a h) =
    object
      [ "status" .= ("playing" :: Text),
        "attempts" .= a,
        "hint" .= h
      ]
  toJSON (Won s a) =
    object
      [ "status" .= ("won" :: Text),
        "attempts" .= a
      ]

-- Request types
data GuessRequest = GuessRequest
  { gameId :: GameId,
    guess :: Int
  }
  deriving (Show)

instance FromJSON GuessRequest where
  parseJSON = withObject "GuessRequest" $ \o ->
    GuessRequest <$> o .: "gameId" <*> o .: "guess"

-- Helper to generate a new game ID using UUID
newGameId :: IO GameId
newGameId = T.pack . show <$> nextRandom

-- API routes
app :: GameStore -> ScottyM ()
app store = do
  -- Serve static files
  get "/" $ file "static/index.html"
  get "/style.css" $ file "static/style.css"
  get "/app.js" $ file "static/app.js"

  -- Start a new game
  post "/api/start" $ do
    secret <- liftIO $ randomRIO (1, 100)
    gameId <- liftIO newGameId
    let state = applyEvent NotStarted (StartGame secret)
    liftIO $ modifyMVar_ store $ \m ->
      return $ Map.insert gameId state m
    json $
      object
        [ "gameId" .= gameId,
          "state" .= state
        ]

  -- Make a guess
  post "/api/guess" $ do
    req <- jsonData
    mState <-
      liftIO $
        readMVar store >>= \m ->
          return $ Map.lookup (gameId req) m
    case mState of
      Nothing -> do
        status status404
        json $ object ["error" .= ("Game not found" :: Text)]
      Just state -> do
        let newState = applyEvent state (Guess (guess req))
        liftIO $ modifyMVar_ store $ \m ->
          return $ Map.insert (gameId req) newState m
        json $ object ["state" .= newState]

  -- Get current game state
  get "/api/state/:gameId" $ do
    gid <- pathParam "gameId"
    mState <-
      liftIO $
        readMVar store >>= \m ->
          return $ Map.lookup gid m
    case mState of
      Nothing -> do
        status status404
        json $ object ["error" .= ("Game not found" :: Text)]
      Just state -> json $ object ["state" .= state]

  -- Health check endpoint
  get "/health" $ do
    json $ object ["status" .= ("ok" :: Text)]

main :: IO ()
main = do
  -- Get port from environment variable or default to 3000
  portEnv <- lookupEnv "PORT"
  let port = case portEnv of
        Just p -> read p
        Nothing -> 3000
  putStrLn $ "Starting web server on port " ++ show port
  store <- newMVar Map.empty
  scotty port $ app store
