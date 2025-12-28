module Main where

greet :: String -> String
greet name =
  "Hello, " ++ name ++ "! Welcome to Haskell."

main :: IO ()
main = do
  let message = greet "George"
  putStrLn message
