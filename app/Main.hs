module Main where

import           Lib
import qualified Options.Applicative as OA

main :: IO ()
main = do
  cmd <- OA.execParser (OA.info (OA.helper <*> programOpts) programInfo)
  print cmd
  interpret cmd
  return ()
