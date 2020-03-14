-- like Memory2, but with state monad written with hand

module Storage.Memory where
-- import qualified Data.Map as Map
import Control.Monad.Trans.State
import Domain.Model
import UseCases.Client

type Objects = [(Id, Object)]

type MemoryStorage a = State Objects a

-- newId :: MemoryStorage a -> MemoryStorage Id
-- newId s = do
--     objs <- evalState s
--     return fst $ fst objs 

-- -- with state - return (newId, Objects)
-- -- using state monad is common way of processing state.
-- addObject :: Object -> MemoryStorage Objects -> MemoryStorage Objects
-- addObject ob store = do
--     id <- newId store
--     return (id,ob):s

-- getObject :: Id -> Objects -> Maybe Object
-- getObject _ [] = Nothing
-- getObject id (o:objs) = if (fst o) == id 
--                         then Just . snd $ o
--                         else getObject id objs



-- instance Storage (MemoryStorage a) where
--     getById = getObject
--     save = addObject
--     init = MemoryStorage []
    
-- store = MemoryStorage [] :: MemoryStorage Objects