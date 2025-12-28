module Main where

import System.Random (randomRIO)

-- The state of our game
data GameState
  = NotStarted
  | Playing
      { secret :: Int,
        attempts :: Int,
        lastHint :: Maybe String
      }
  | Won
      { secret :: Int,
        attempts :: Int
      }
  deriving (Show)

-- Events that can happen in the game
data GameEvent
  = StartGame Int -- secret number
  | Guess Int -- player's guess
  deriving (Show)

applyEvent :: GameState -> GameEvent -> GameState
applyEvent NotStarted (StartGame secret) =
  Playing secret 0 Nothing
applyEvent (Playing secret attempts _) (Guess n)
  | n == secret =
      Won secret (attempts + 1)
  | n < secret =
      Playing secret (attempts + 1) (Just "Too low!")
  | otherwise =
      Playing secret (attempts + 1) (Just "Too high!")
applyEvent state _ = state

render :: GameState -> IO ()
render NotStarted =
  putStrLn "Welcome! Type 'start' to begin."
render (Playing _ attempts hint) = do
  putStrLn ("Attempts so far: " ++ show attempts)
  case hint of
    Just h -> putStrLn h
    Nothing -> pure ()
  putStrLn "Enter your guess:"
render (Won _ attempts) =
  putStrLn ("You won in " ++ show attempts ++ " attempts!")

parseInt :: String -> Maybe Int
parseInt s =
  case reads s of
    [(n, "")] -> Just n
    _ -> Nothing

readEvent :: GameState -> IO GameEvent
readEvent NotStarted = do
  secret <- randomRIO (1, 100)
  putStrLn "Game started! Guess a number between 1 and 100."
  pure (StartGame secret)
readEvent (Playing _ _ _) = do
  input <- getLine
  case parseInt input of
    Just n -> pure (Guess n)
    Nothing -> do
      putStrLn "Please enter a valid number."
      readEvent (Playing 0 0 Nothing) -- state doesn't matter here
readEvent (Won _ _) =
  -- no more events once won
  pure (Guess 0)

gameLoop :: GameState -> IO ()
gameLoop state = do
  render state
  case state of
    Won _ _ -> pure ()
    _ -> do
      event <- readEvent state
      let nextState = applyEvent state event
      gameLoop nextState

main :: IO ()
main =
  gameLoop NotStarted
