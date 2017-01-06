
module Executer.BashScript(bashScriptExecuter) where

import Commands.Command

-- 
-- This is a small renderer 
-- which compiles the wishlist into a shell-script
--

bashScriptExecuter :: [ Command ] -> String
bashScriptExecuter commands = 
    foldl commandConcat "#!/bin/bash\nset -x\n" $ map commandSnippet commands
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


