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

main :: IO ()
main = do
  let s0 = NotStarted
  let s1 = applyEvent s0 (StartGame 42)
  let s2 = applyEvent s1 (Guess 10)
  let s3 = applyEvent s2 (Guess 42)

  print s0
  print s1
  print s2
  print s3
