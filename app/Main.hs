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

import qualified Command.Command as Cmd
import Command.Result

helpMessage :: String
helpMessage = "TagMe.\n\
    \Tool for labudubadubdubz.\n\
    \\n\
    \Commands:\n\
    \ - create\n\
    \ - add\n\
    \ - quit\n\
    \ - show\n\
    \ - help"

prompt :: String
prompt = "[tagme]:"

{-|  System (programm) state.
Contains storages, results, and can contain many other parts of state, like redux store.
-}
data System = System {
      objects :: MemoryObjectsStorage
    , tasks :: MemoryTasksStorage
    , getResult :: Maybe Result
}

{-| Inifinitively ask user for command, and execute it. 
Operates on system state, so for first time it's needs 
some initial state to run.
-}
mainProc :: IO System -> IO ()
mainProc sys = do
    putStr prompt
    cmdLine <- getLine
    case Cmd.parseCommand cmdLine of
        Left s -> showResponse s
        Right cmd@Cmd.Quit -> runCommand cmd sys >> pure ()
        Right cmd -> runCommand cmd sys >>= (mainProc . pure)

-- show String if it's not empty 
showResponse :: String -> IO ()
showResponse s = if null s then return () else putStrLn s 

-- | Show result of command execution
displayResult :: Maybe Result -> IO ()
displayResult Nothing = putStrLn "Done"
displayResult (Just r) = print r

{-| Apply command to system state and return new system state
-}
applyCommand :: Cmd.Command -> System -> System

applyCommand (Cmd.CreateTask tn) ss = System objs nts res where
    (_id, nts) = taskSave (tasks ss) (Task tn "")
    res = Just $ TaskCreated _id
    objs = objects ss

applyCommand Cmd.ShowTasks (System os ts _) = System os ts result where
    result = Just . ShowTasks $ storeItems ts 

applyCommand (Cmd.Load _ _) s = s
applyCommand (Cmd.Add _ _) s = s
applyCommand (Cmd.Start _) s = s
applyCommand Cmd.Set s = s
applyCommand Cmd.Finish s = s
applyCommand Cmd.ListObjects s = s
applyCommand Cmd.Help s = s
applyCommand Cmd.Quit s = s

{-| evaluate command and show command result.
I think, it's needed 'couze i don't konw how to work with state monad 
-}
runCommand :: Cmd.Command -> IO System -> IO System
runCommand Cmd.Help ss' = putStrLn helpMessage >> ss'
runCommand Cmd.Quit ss' = putStrLn "Bye." >> ss'
-- runCommand cmd ss' = fmap (applyCommand cmd) ss' >>= (\ss -> (displayResult . result) ss >> pure ss)
runCommand cmd ss' = do
    system <- fmap (applyCommand cmd) ss'
    displayResult $ getResult system
    pure system

main :: IO ()
main = do
    putStrLn helpMessage
    mainProc $ pure $ System emptyObjectsStore emptyTasksStore Nothing