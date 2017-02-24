{-# LANGUAGE FlexibleInstances #-}


{-|

Module      : Azubi.StateExecutor
Description : Core low level State evaluation and enforcement
Copyright   : (c) Ingolf Wagner, 2017
License     : GPL-3
Maintainer  : azubi@ingolf-wagner.de
Stability   : experimental
Portability : POSIX

'State' must be evaluated and enforced.
This is done by a 'StateExecutor'.

-}
module Azubi.StateExecutor where

import Azubi.Model

{-|

should evaluate and enforce given 'State's

-}
class StateExecutor a where
  execute :: a -> [State] -> IO ()

{-|

should create a script.

todo : write one

-}
class ScriptStateExecuter a where
  header :: a -> IO ()
  footer :: a -> IO ()
  body   :: a -> [State] -> IO ()

-- | wrapper type to prove Haskell
-- there will be now looping.
newtype ScriptExecute a = ScriptExecute a

instance ScriptStateExecuter a => StateExecutor (ScriptExecute a) where
  execute (ScriptExecute context) states = do
    header context
    body context states
    footer context






{-|

should /run/ the states on the local machine.

-}
class LocalStateExecute a where
  setup    :: a -> IO ()
  executeState :: a -> State -> IO StateResult
  tearDown :: a -> [StateResult] -> IO ()

-- | wrapper type to prove Haskell
-- there will be now looping.
newtype LocalContext a = LocalContext a

instance LocalStateExecute a => StateExecutor (LocalContext a) where

  execute (LocalContext context) states = do
    setup context
    results <- collectStateResults states
    tearDown context results
    where
      collectStateResults :: [State] -> IO [StateResult]
      collectStateResults [] = return []
      collectStateResults (x:xs) = do
        result <- executeState context x
        restResults <- collectStateResults xs
        return $ result:restResults




