
module Azubi.Commands.Existance where


import Azubi.Core.Revertable
import Azubi.Core.Command


class Existance a where
  exists :: (Revertable b) => a -> b -> [ Command ]
  exists c revertable =
    if (isRevert revertable) then
      notExists c revertable
    else
      doExists c revertable

  notExists :: (Revertable b) => a -> b -> [ Command ]
  doExists ::  (Revertable b) => a -> b -> [ Command ]
