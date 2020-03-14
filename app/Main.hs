-- add new object
-- update object
-- remove object

import UseCases.Client
import Storage.Memory2
import Domain.Model
{-
func :: m a -> m b
void $ func a === func a >> pure ()
-}
import Control.Monad (void, liftM)

helpMessage = "TagMe.\n\
    \Tool for labudubadubdubz.\n\
    \\n\
    \Commands:\n\
    \ - create\n\
    \ - add\n\
    \ - quit\n\
    \ - show\n\
    \ - help"

prompt = "[tagme]:"

{-|  System (programm) state.
Contains storages, results, and can contain many other parts of state, like redux store.
-}
data System = System {
      objects :: MemoryObjectsStorage
    , tasks :: MemoryTasksStorage
    , result :: Maybe Result
}


data Command = Create {taskName :: String} -- task  
             | Add {task :: Task } -- object
             | Start -- task
             | Set -- mark
             | Finish -- object 
             | Show -- tasks
             | Help -- show help
             | Quit -- exit program
    deriving (Show, Eq)


data Result = TaskCreated Id
            | TaskStarted Id
            | MarkSetted
            | ShowTasks [StoreItem Task]
            | Error String

instance Show Result where
    show (TaskCreated id) = "Task created: " ++ show id
    show (TaskStarted id) = "Task started: " ++ show id
    show (MarkSetted) = "Mark setted"
    show (ShowTasks []) = "No tasks there"
    show (ShowTasks tasks) = "Tasks: \n" ++ unlines (map (pad . show) tasks)
                                where pad s = "\t" ++ s 

main :: IO ()
main = do
    putStrLn helpMessage
    mainProc $ pure $ System emptyObjectsStore emptyTasksStore Nothing



{-| Inifinitively ask user for command, and execute it. 
Operates on system state, so for first time it's needs 
some initial state to run.
-}
mainProc sys = do
    putStr prompt
    cmdLine <- getLine
    case parseCommand cmdLine of
        Left s -> showResponse s
        Right cmd@(Quit) -> runCommand cmd sys >> pure ()
        Right cmd -> runCommand cmd sys >>= (mainProc . pure)

-- show String if it's not empty 
showResponse :: String -> IO ()
showResponse s = if null s then return () else putStrLn s 


{-| Apply command to system state and return new system state
-}
applyCommand :: Command -> System -> System
applyCommand (Create tn) ss = System objs nts res where
    (_id, nts) = taskSave (tasks ss) (Task tn "")
    res = Just $ TaskCreated _id
    (objs) = objects ss
applyCommand (Show) (System os ts _) = System os ts result where
    result = Just . ShowTasks $ storeItems ts 

{-| Show result of command execution
-}
displayResult :: Maybe Result -> IO ()
displayResult Nothing = putStrLn "Done"
displayResult (Just r) = print r

{-| evaluate command and show command result.
I think, it's needed 'couze i don't konw how to work with state monad 
-}
runCommand :: Command -> IO System -> IO System
runCommand Help ss' = putStrLn helpMessage >> ss'
runCommand Quit ss' = putStrLn "Bye." >> ss'
runCommand cmd ss' = fmap (applyCommand cmd) ss' >>= (\ss -> (displayResult . result) ss >> pure ss)


{-| parse command line and generate Command object that can be evaluated
If parse fails - returns @Left "error string"@ 
-}
parseCommand :: String -> Either String Command
parseCommand s = let (cmd:args) = words s 
                 in case cmd of
                    "create"    -> Right $ Create taskName
                                    where (taskName:_) = args
                    "add"       -> Right $ Add (Task n i) 
                                    where (n:i:_) = args
                    "show"      -> Right Show
                    "quit"      -> Right Quit
                    "help"      -> Right Help
                    s           -> Left $ "Unknown command: " ++ show s ++ ". Use 'help' or 'quit' if you don't know what to do."