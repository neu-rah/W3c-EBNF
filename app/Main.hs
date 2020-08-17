module Main where

import Lib
import Control.Monad
import System.IO
import System.TimeIt
-- import GenParser -- this file is not ready yet, it should allow generating haskell source to parse the given grammar


main :: IO ()
main = forever $ do
  putStr "test> "
  hFlush stdout
  xp<-getLine
  putStr $ "parsing " <> xp <> "\n"
  timeIt(print $ run testParser xp)
  -- timeIt(print $ run xpath_parser xp)
