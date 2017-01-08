

module Core.Syntax where


import Core.Command
import Core.Context
import Core.Revertable

type AzubiName = String
type  CommandContext a =  a -> [ Command ]

data CommandContainer a = CommandContainer a [ Command ]

-- | call command creation function with context
(&) :: (Context a) => CommandContainer a -> CommandContext a -> CommandContainer a
(CommandContainer a commands) & f = CommandContainer a $ commands ++ (f a )

-- | call command creation function with reverted context
(!) :: (Revertable a) => CommandContainer a -> CommandContext a -> CommandContainer a
(CommandContainer a commands) ! f = CommandContainer a $ commands ++ (f (toggleRevert a))

azubi :: (Context a ) => a -> CommandContainer a
azubi context = CommandContainer context []


data RuntimeContext
