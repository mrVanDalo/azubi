

-- to manage reverting of commands

module Core.Revertable where

import Core.Context


-- | Revertable context is for to implement
-- | revertable Actions
class Context a => Revertable a where

  -- | toggle reverting
  toggleRevert :: a -> a

  -- | should revert an action
  -- | eg: uninstall
  isRevert :: a -> Bool

  setRevert :: a -> a
  setRevert context =
    if (isRevert context)
    then context
    else toggleRevert context

  -- | should execute an action
  -- | eg: install
  isExecute :: a -> Bool
  isExecute context = not $ isRevert context

  setExectue :: a -> a
  setExectue context =
    if (isExecute context)
    then context
    else toggleRevert context

