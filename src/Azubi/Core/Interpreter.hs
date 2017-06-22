{-# LANGUAGE FlexibleInstances #-}

{-|

Module      : Azubi.Core.Interpreter
Description : Core low level State evaluation and enforcement
Copyright   : (c) Ingolf Wagner, 2017
License     : GPL-3
Maintainer  : azubi@ingolf-wagner.de
Stability   : experimental
Portability : POSIX

'State' must be evaluated and enforced.
This is done by a 'Interpreter'.

-}
module Azubi.Core.Interpreter where

import           Azubi.Core.Model

{-|

should evaluate and enforce given 'State's

-}
class Interpreter a where
  execute :: a -> [State] -> IO ()

{-|

should create a script.

todo : write one

-}
class ScriptInterpreter a where
  header :: a -> IO ()
  footer :: a -> IO ()
  body :: a -> [State] -> IO ()

-- | wrapper type to prove Haskell
-- there will be now looping.
newtype ScriptExecute a =
  ScriptExecute a

instance ScriptInterpreter a => Interpreter (ScriptExecute a) where
  execute (ScriptExecute context) statesToFulfull = do
    header context
    body context statesToFulfull
    footer context

{-|

should /run/ the states on the local machine.

-}
class LocalInterpreter a
  -- | a setup function that can be overwritten
  -- for stuff that has to be done before
  -- processing the states
                           where
  setup :: a -> IO ()
  setup _ = return ()
  -- | the function that should execute all states
  -- this one has to be implemented
  executeState :: a -> State -> IO StateResult
  -- | the function called right after processing
  -- all of the states which can be overwritten
  tearDown :: a -> [StateResult] -> IO ()
  tearDown _ _ = return ()
  -- | will be called right before the
  -- `exectueState` so you can preprocess the State
  -- and remove or add stuff
  prePorcessState :: a -> State -> IO State
  prePorcessState _ = return

-- | wrapper type to prove Haskell
-- there will be now looping.
newtype LocalContext a =
  LocalContext a

instance LocalInterpreter a => Interpreter (LocalContext a) where
  execute (LocalContext context) statesToFulfull = do
    setup context
    processedStates <- collectPreprocessed statesToFulfull
    results <- collectStateResults processedStates
    tearDown context results
    where
      collectStateResults :: [State] -> IO [StateResult]
      collectStateResults [] = return []
      collectStateResults (x:xs) = do
        result <- executeState context x
        restResults <- collectStateResults xs
        return $ result : restResults
      collectPreprocessed :: [State] -> IO [State]
      collectPreprocessed [] = return []
      collectPreprocessed (x:xs) = do
        result <- prePorcessState context x
        restResults <- collectPreprocessed xs
        return $ result : restResults
