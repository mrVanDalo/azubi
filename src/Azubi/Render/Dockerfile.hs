
-- das ganze macht wenig sinn auf der ebene, 
-- es wäre besser das als system zu machen

module Executer.Dockerfile(dockerfileExecuter) where

import Commands.Command

dockerfileExecuter :: [ Command ] -> String
dockerfileExecuter commands = 
    foldl commandConcat "FROM <unknown>\n" $ 
        map withRunPrefix $ 
        map commandSnippet commands
    where
        commandConcat a b = a ++ "\n\n" ++ b
        withRunPrefix a = "RUN " ++ a


commandSnippet :: Command -> String

commandSnippet (IfCommand (BoolCommand b) t e) = 
    "if [[ " ++ b ++ " ]] ;then \\\n\t" ++ 
    (commandSnippet t) ++ 
    " ; else \\\n\t" ++ 
    (commandSnippet e) ++ " ; fi"

commandSnippet (Command c) = c
