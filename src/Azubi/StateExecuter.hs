{-# LANGUAGE FlexibleInstances #-}

module Azubi.StateExecuter where

import Azubi.Model





-- | core main executor
class StateExecuter a where
  execute :: a -> [State] -> IO ()

-- | script executor class
-- todo : write one
class ScriptStateExecuter a where
  header :: a -> IO ()
  footer :: a -> IO ()
  body   :: a -> [State] -> IO ()

newtype ScriptExecute a = ScriptExecute a

instance ScriptStateExecuter a => StateExecuter (ScriptExecute a) where
  execute (ScriptExecute context) states = do
    header context
    body context states
    footer context


-- | local executor class
class LocalStateExecute a where
  setup    :: a -> IO ()
  run      :: a -> State -> IO StateResult
  tearDown :: a -> [StateResult] -> IO ()


newtype LocalContext a = LocalContext a

instance LocalStateExecute a => StateExecuter (LocalContext a) where

  execute (LocalContext context) states = do
    setup context
    results <- collectStateResults states
    tearDown context results
    where
      collectStateResults :: [State] -> IO [StateResult]
      collectStateResults [] = return []
      collectStateResults (x:xs) = do
        result <- run context x
        restResults <- collectStateResults xs
        return $ result:restResults




