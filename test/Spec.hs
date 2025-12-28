module Main where

import Game

assert :: Bool -> String -> IO ()
assert True _ = pure ()
assert False msg = error msg

testStartGame :: IO ()
testStartGame = do
  let state = applyEvent NotStarted (StartGame 42)
  assert (attempts state == 0) "Game should start with 0 attempts"

testWrongGuess :: IO ()
testWrongGuess = do
  let state1 = applyEvent NotStarted (StartGame 10)
  let state2 = applyEvent state1 (Guess 3)
  assert (attempts state2 == 1) "Attempts should increase"
  assert (lastHint state2 == Just "Too low!") "Hint should be 'Too low!'"

testWinningGuess :: IO ()
testWinningGuess = do
  let state1 = applyEvent NotStarted (StartGame 7)
  let state2 = applyEvent state1 (Guess 7)
  case state2 of
    Won _ attempts ->
      assert (attempts == 1) "Winning attempt count incorrect"
    _ ->
      error "Game should be in Won state"

main :: IO ()
main = do
  testStartGame
  testWrongGuess
  testWinningGuess
  putStrLn "All tests passed!"
