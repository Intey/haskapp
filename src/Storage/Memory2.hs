{-# LANGUAGE FlexibleInstances #-}

module Storage.Memory2 
    (
        MemoryStorage,
        storeItems,
        store,
        MemoryObjectsStorage,
        MemoryTasksStorage
    )
    where

-- import qualified Data.Map as Map
import Domain.Model
import UseCases.Client


_newId :: [StoreItem i] -> Id
_newId s
    | null s = 1
    | otherwise = (1+) . fst . head $ s


newtype MemoryStorage i = MemoryStorage { items :: [StoreItem i]} deriving (Show)
storeItems :: MemoryStorage i -> [StoreItem i]
storeItems = reverse . items

type MemoryObjectsStorage = MemoryStorage Object
type MemoryTasksStorage = MemoryStorage Task

store :: [StoreItem i] -> MemoryStorage i
store = MemoryStorage

-- TODO: multiparams typeclasses or type families for reduce duplications and different names of functions

instance ObjectsStorage MemoryObjectsStorage where
    objectNewId = _newId . items
    objectGetAll = (map snd) . storeItems
    objectGetById = memGetById . items
    objectSave s t = (createdId, changedStore)
                      where changedStore = store $ (objectNewId s, t):(items s)
                            createdId = fst . head . items $ changedStore
    emptyObjectsStore = MemoryStorage []

instance TaskStorage MemoryTasksStorage where
    taskNewId = _newId . items
    taskGetAll = (map snd) . storeItems
    taskGetById = memGetById . items
    taskSave s t = (createdId, changedStore)
                    where changedStore = store $ (taskNewId s, t):(items s)
                          createdId = fst . head . items $ changedStore
    emptyTasksStore = MemoryStorage []


memGetById :: [StoreItem o] -> Id -> Maybe o
memGetById [] _ = Nothing
memGetById (o:objs) i = if (fst o) == i
                        then Just . snd $ o
                        else memGetById objs i  