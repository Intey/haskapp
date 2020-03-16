module Command.Command(
    Command(..)
    , parseCommand
) where

data Command = CreateTask {getTaskName :: String} -- task  
             | Load { getObjName :: String, getContentURI :: String } --loa
             | Add { getTaskId :: Int, getObjectId :: Int }
             | Start { getTaskName :: String } -- task
             | Set -- mark
             | Finish -- object 
             | ShowTasks -- tasks
             | ListObjects
             | Help -- show help
             | Quit -- exit program
    deriving (Show, Eq)

{-| parse command line and generate Command object that can be evaluated
If parse fails - returns @Left "error string"@ 
-}
parseCommand :: String -> Either String Command
parseCommand s = let (cmd:args) = words s 
                 in case cmd of
                    "create"    -> Right $ CreateTask taskName
                                    where (taskName:_) = args
                    "add"       -> parseAdd args
                    "show"      -> Right ShowTasks
                    "quit"      -> Right Quit
                    "help"      -> Right Help
                    s'           -> Left $ "Unknown command: " ++ show s' ++ ". Use 'help' or 'quit' if you don't know what to do."




parseAdd :: [String] -> Either String Command 
parseAdd [t,o] = Right $ Add (read t :: Int) (read o :: Int)
parseAdd _ = Left "add command gets 2 arguments: task id and object id "