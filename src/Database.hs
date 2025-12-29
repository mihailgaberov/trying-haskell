module Database
  ( LeaderboardEntry (..),
    initDatabase,
    addEntry,
    getTopEntries,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.Types (Query (..))

-- Leaderboard entry data type
data LeaderboardEntry = LeaderboardEntry
  { entryId :: Int,
    entryName :: Text,
    entryAttempts :: Int,
    entrySecret :: Int,
    entryTimestamp :: Text
  }
  deriving (Show, Eq)

-- SQLite instances
instance FromRow LeaderboardEntry where
  fromRow = LeaderboardEntry <$> field <*> field <*> field <*> field <*> field

instance ToRow LeaderboardEntry where
  toRow entry =
    toRow
      ( entryName entry,
        entryAttempts entry,
        entrySecret entry,
        entryTimestamp entry
      )

-- Database file path
dbPath :: FilePath
dbPath = "leaderboard.db"

-- Initialize database and create table if it doesn't exist
initDatabase :: IO ()
initDatabase = do
  conn <- open dbPath
  execute_
    conn
    (Query $ T.pack "CREATE TABLE IF NOT EXISTS leaderboard (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL, attempts INTEGER NOT NULL, secret INTEGER NOT NULL, timestamp TEXT NOT NULL)")
  close conn

-- Add a new entry to the leaderboard
addEntry :: Text -> Int -> Int -> IO ()
addEntry name attempts secret = do
  conn <- open dbPath
  timestamp <- getCurrentTimestamp
  execute
    conn
    (Query $ T.pack "INSERT INTO leaderboard (name, attempts, secret, timestamp) VALUES (?, ?, ?, ?)")
    (name, attempts, secret, timestamp)
  close conn

-- Get top N entries, sorted by attempts (ascending), then by timestamp (ascending)
getTopEntries :: Int -> IO [LeaderboardEntry]
getTopEntries limit = do
  conn <- open dbPath
  entries <-
    query
      conn
      (Query $ T.pack "SELECT id, name, attempts, secret, timestamp FROM leaderboard ORDER BY attempts ASC, timestamp ASC LIMIT ?")
      (Only limit)
  close conn
  return entries

-- Helper to get current timestamp as ISO 8601 string
getCurrentTimestamp :: IO Text
getCurrentTimestamp = do
  now <- getCurrentTime
  return $ T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now

