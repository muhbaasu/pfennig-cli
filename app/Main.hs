module Main where

import           Control.Monad.Reader (runReaderT)
import           Lib
import qualified Options.Applicative  as OA

main :: IO ()
main = do
  cmd <- OA.execParser (OA.info (OA.helper <*> programOpts) programInfo)
  runReaderT (interpret cmd) defaultGlobalOptions
