module Command.Result (
    Result(..)
) where

import Domain.Model
import UseCases.Client (StoreItem)

data Result = TaskCreated Id
        | TaskStarted Id
        | MarkSetted
        | ShowTasks [StoreItem Task]
        | Error String


instance Show Result where
    show (TaskCreated i) = "Task created: " ++ show i
    show (TaskStarted i) = "Task started: " ++ show i
    show MarkSetted = "Mark setted"
    show (ShowTasks []) = "No tasks there"
    show (ShowTasks tasks) = "Tasks: \n" ++ unlines (map (pad . show) tasks)
                                where pad s = "\t" ++ s 
    show (Error msg) = "Error: " ++ msg

