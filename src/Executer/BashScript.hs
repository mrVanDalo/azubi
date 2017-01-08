
module Executer.BashScript(bashScriptExecuter) where

import Core.Command
import Core.Syntax

--
-- This is a small renderer
-- which compiles the wishlist into a shell-script
--

bashScriptExecuter :: CommandContainer a -> String
bashScriptExecuter (CommandContainer _ commands) =
    foldl commandConcat "#!/bin/bash\n" $ map commandSnippet commands
    where
        commandConcat a b = a ++ "\n" ++ b


commandSnippet :: Command -> String
commandSnippet (ShellCommand c)  = c
commandSnippet (InfoMsg i)  = "echo 'INFO : " ++ i ++ "'"
commandSnippet (ErrorMsg i) = "echo 'ERROR: " ++ i ++ "'"
commandSnippet (IfCommand (BoolCommand b) t e) =
    "if [[ " ++ b ++ " ]]\n" ++
    "then\n" ++
    (unlines (map commandSnippet t)) ++
    "else\n" ++
    (unlines (map commandSnippet e)) ++ "fi\n"
commandSnippet (FileContent path content) =
  "cat >" ++ path ++ " <<EOF\n" ++ unlines content ++ "EOF\n"

