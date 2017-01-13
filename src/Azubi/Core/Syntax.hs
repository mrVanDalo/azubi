

module Azubi.Core.Syntax where

import Azubi.Core.Command
import Azubi.Core.Context
import Azubi.Core.Revertable

-- | wrap up everything
azubiConfig  :: (Context a) => a -> [(a -> [Command])] -> [Command]
azubiConfig con commands =
  concat $  map injectContext commands
  where
    injectContext f = f con

-- | create a submodule
submodule :: (Context a) => [(a -> [Command])] -> a -> [Command]
submodule commands context =
  concat $ map injectContext commands
  where
    injectContext f = f context

requires :: (Context a) => (a -> [Command]) -> (a -> [Command])  -> a -> [Command]
first `requires` sec = \context ->
                          [ Dependency { body=(first context)
                                       , dependency=(sec context)}]


-- | add a command
(&) :: (Context a) => [ (a -> [Command]) ] -> (a -> [Command] ) -> [ (a -> [Command])]
first & second = first ++ [ second ]


-- | add the opposite of the command
(!) :: (Revertable a) => [ (a -> [Command]) ] -> (a -> [Command] ) -> [ (a -> [Command])]
first ! second = first ++ [( second . toggleRevert ) ]


-- | add a command under condition of being in
-- | a positive context
(&?&) :: (Revertable a) => [ (a -> [Command]) ] -> (a -> [Command] ) -> [ (a -> [Command])]
first &?& second = first ++ [ check ]
  where check con =
          if (isRevert con)
          then []
          else second con

-- | add the opposite command under condition of being in
-- | a positive context
(!?&) :: (Revertable a) => [ (a -> [Command]) ] -> (a -> [Command] ) -> [ (a -> [Command])]
first !?& second = first ++ [ check ]
  where check con =
          if (isRevert con)
          then []
          else second (setRevert con)

-- | add a command under condition of being in
-- | a negative context
(&?!) :: (Revertable a) => [ (a -> [Command]) ] -> (a -> [Command] ) -> [ (a -> [Command])]
first &?! second = first ++ [ check ]
  where check con =
          if (isRevert con)
          then second (setExectue con)
          else []

-- | add the opposite command under condition of being in
-- | a negative context
(!?!) :: (Revertable a) => [ (a -> [Command]) ] -> (a -> [Command] ) -> [ (a -> [Command])]
first !?! second = first ++ [ check ]
  where check con =
          if (isRevert con)
          then second con
          else []
