

-- to manage reverting of commands

module Core.Revertable where

import Core.Context

class Context a => Revertable a where

  -- returns if the context should be reverted
  isRevert :: a -> Bool

  -- toggle reverting
  toggleRevert :: a -> a
