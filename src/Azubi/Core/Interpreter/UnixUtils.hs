module Azubi.Core.Interpreter.UnixUtils
  ( preProcessors
  , PreProcessors(..)
  ) where

import           System.Directory
import           System.FilePath.Posix

{-|

A container that holds context and functions
to update States.

-}
newtype PreProcessors = PreProcessors
  { homeUpdate :: String -> String
  }

{-|

proper constructor.

-}
preProcessors :: IO PreProcessors
preProcessors = do
  home <- getHomeDirectory
  return $ PreProcessors $ homeUpdateImpl home

{-|

replace ~ at the start of path.
Will not touch ~ inside of a path.

-}
homeUpdateImpl :: String -> String -> String
homeUpdateImpl home path =
  if head (splitDirectories path) == "~"
    then joinPath $ home : drop 1 (splitDirectories path)
    else path
