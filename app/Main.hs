module Main where

import Game
import System.Random (randomRIO)

render :: GameState -> IO ()
render NotStarted =
  putStrLn "Welcome! Press Enter to start."
render (Playing _ attempts lastHint) = do
  putStrLn ("Attempts so far: " ++ show attempts)
  case lastHint of
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
  _ <- getLine
  secret <- randomRIO (1, 100)
  putStrLn "Game started! Guess a number between 1 and 100."
  pure (StartGame secret)
readEvent (Playing _ _ _) = do
  input <- getLine
  case parseInt input of
    Just n -> pure (Guess n)
    Nothing -> do
      putStrLn "Please enter a valid number."
      readEvent (Playing 0 0 Nothing)
readEvent (Won _ _) =
  pure (Guess 0)

gameLoop :: GameState -> IO ()
gameLoop state = do
  render state
  case state of
    Won _ _ -> pure ()
    _ -> do
      event <- readEvent state
      gameLoop (applyEvent state event)

main :: IO ()
main =
  gameLoop NotStarted
