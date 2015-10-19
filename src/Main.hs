module Main where

import Parser

import Control.Monad.IO.Class
import System.Console.Haskeline

process :: String -> IO ()
process line = do
  let parsed = parseExpr line
  case parsed of
    Left error -> print error
    Right expression -> print expression

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    input <- getInputLine "ζ "
    case input of 
      Nothing -> outputStrLn "αντίο!"
      Just line -> liftIO (process line) >> loop
