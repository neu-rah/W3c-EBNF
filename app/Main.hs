module Main where

import Lib
import Control.Monad
import System.IO
import System.TimeIt
import GenParser

main :: IO ()
main = forever $ do
  putStr "test> "
  hFlush stdout
  xp<-getLine
  putStr $ "parsing " <> xp <> "\n"
  timeIt(print $ run testParser xp)
  -- timeIt(print $ run xpath_parser xp)
