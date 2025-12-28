module Game
  ( GameState (..),
    GameEvent (..),
    applyEvent,
  )
where

-- Game state
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

-- Game events
data GameEvent
  = StartGame Int
  | Guess Int
  deriving (Show)

-- Pure state transition
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
applyEvent state _ =
  state
