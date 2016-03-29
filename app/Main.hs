module Main where

import           Lib

main :: IO ()
main = do
  cmd <- parseArgs
  runCommand cmd
