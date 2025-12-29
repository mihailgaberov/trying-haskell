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
import Database
import Game
import Network.HTTP.Types.Status (status400, status404)
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
        "attempts" .= a,
        "secret" .= s
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

-- JSON instance for LeaderboardEntry
instance ToJSON LeaderboardEntry where
  toJSON entry =
    object
      [ "id" .= entryId entry,
        "name" .= entryName entry,
        "attempts" .= entryAttempts entry,
        "secret" .= entrySecret entry,
        "timestamp" .= entryTimestamp entry
      ]

-- Request type for submitting leaderboard entry
data LeaderboardRequest = LeaderboardRequest
  { leaderboardName :: Text,
    leaderboardAttempts :: Int,
    leaderboardSecret :: Int
  }
  deriving (Show)

instance FromJSON LeaderboardRequest where
  parseJSON = withObject "LeaderboardRequest" $ \o ->
    LeaderboardRequest <$> o .: "name" <*> o .: "attempts" <*> o .: "secret"

-- Sanitize and validate name (max 10 chars, alphanumeric + spaces)
sanitizeName :: Text -> Maybe Text
sanitizeName name =
  let trimmed = T.strip name
      sanitized = T.take 10 $ T.filter (\c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == ' ') trimmed
   in if T.null sanitized || T.length sanitized < 1
        then Nothing
        else Just sanitized

-- Helper to generate a new game ID using UUID
newGameId :: IO GameId
newGameId = T.pack . show <$> nextRandom

-- API routes
app :: GameStore -> ScottyM ()
app store = do
  -- Serve static files
  get "/" $ file "static/index.html"
  get "/style.css" $ do
    setHeader "Content-Type" "text/css"
    file "static/style.css"
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

  -- Submit leaderboard entry
  post "/api/leaderboard" $ do
    req <- jsonData
    case sanitizeName (leaderboardName req) of
      Nothing -> do
        status status400
        json $ object ["error" .= ("Invalid name. Must be 1-10 alphanumeric characters." :: Text)]
      Just sanitizedName -> do
        liftIO $ addEntry sanitizedName (leaderboardAttempts req) (leaderboardSecret req)
        json $ object ["status" .= ("ok" :: Text)]

  -- Get leaderboard (top 10)
  get "/api/leaderboard" $ do
    entries <- liftIO $ getTopEntries 10
    json $ object ["entries" .= entries]

  -- Health check endpoint
  get "/health" $ do
    json $ object ["status" .= ("ok" :: Text)]

main :: IO ()
main = do
  -- Initialize database
  initDatabase
  putStrLn "Database initialized"
  
  -- Get port from environment variable or default to 3000
  portEnv <- lookupEnv "PORT"
  let port = case portEnv of
        Just p -> read p
        Nothing -> 3000
  putStrLn $ "Starting web server on port " ++ show port
  store <- newMVar Map.empty
  scotty port $ app store
