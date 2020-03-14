module UseCases.Client where

import Domain.Model

type StoreItem i = (Id, i)

class View a where
    render :: a -> b

class ObjectsStorage store where
    objectNewId :: store -> Id
    objectGetById :: store -> Id -> Maybe Object
    objectSave :: store -> Object -> (Id, store)
    objectGetAll :: store -> [Object]
    emptyObjectsStore :: store

-- viewObjects :: (Storage store, View view) => store -> view -> b
-- viewObjects = render . getAll

marked :: Object -> Bool
marked = undefined

selectUnmarked :: (ObjectsStorage store) => store -> Maybe Object
selectUnmarked s = let unmarkedObjs = filter unmarked $ objectGetAll s
                       unmarked = not . marked
                    in if null unmarkedObjs then Nothing
                       else Just . head $ unmarkedObjs


pickObjectToMarkup :: (ObjectsStorage store) => store -> Either String o
pickObjectToMarkup = undefined


class TaskStorage store where
    taskNewId :: store -> Id
    taskGetById :: store -> Id -> Maybe Task
    taskSave :: store -> Task -> (Id, store)
    taskGetAll :: store -> [Task]
    emptyTasksStore :: store