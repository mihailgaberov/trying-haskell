module Main where

-- The state of our game
data GameState
  = NotStarted
  | Playing {secret :: Int, attempts :: Int}
  | Won {secret :: Int, attempts :: Int}
  deriving (Show)

-- Events that can happen in the game
data GameEvent
  = StartGame Int -- secret number
  | Guess Int -- player's guess
  deriving (Show)

applyEvent :: GameState -> GameEvent -> GameState
applyEvent NotStarted (StartGame secret) =
  Playing {secret = secret, attempts = 0}
applyEvent (Playing secret attempts) (Guess n)
  | n == secret = Won {secret = secret, attempts = attempts + 1}
  | otherwise = Playing {secret = secret, attempts = attempts + 1}
-- Ignore invalid events (no state change)
applyEvent state _ = state

render :: GameState -> IO ()
render NotStarted =
  putStrLn "Welcome! Type 'start' to begin."
render (Playing _ attempts) =
  putStrLn ("Make a guess! Attempts so far: " ++ show attempts)
render (Won _ attempts) =
  putStrLn ("You won in " ++ show attempts ++ " attempts!")

readEvent :: GameState -> IO GameEvent
readEvent NotStarted = do
  _ <- getLine
  -- for now, fixed secret (we’ll improve this later)
  pure (StartGame 42)
readEvent (Playing _ _) = do
  input <- getLine
  pure (Guess (read input))
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
