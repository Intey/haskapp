module Domain.Model where

type Id = Int
type Username = String
type Error = String

data User = User { userId   :: Id
                 , userName :: String
} deriving (Show, Read)

data Object = Doc       {objName :: String }
            | Dialog    {objName :: String }
    deriving (Show, Eq)

newtype Attribute = Attribute String 

data Entity = Entity String [Attribute]

newtype Position a = Position a

data Mark position = Mark position Entity User

data MarkedObject position = MarkedObject [(Mark position)] Object

data Task = Task {
      taskName :: String
    , taskInstruction :: String
    -- , taskObjects :: [Object]   
} deriving (Show, Eq)

-- permission models by departments

newtype Permission = Permission String deriving (Show, Read)
data Group = Group String [Permission] deriving (Show, Read)
data Department = Department Id String


data MarkedObjectP p = MarkedObjectP (MarkedObject p) Department
data DUser = DUser User Department

accessible :: DUser -> MarkedObjectP p -> Bool
accessible (DUser _ (Department i _)) (MarkedObjectP _ (Department i' _)) = i == i'


-- apped 
markup :: User -> Mark position -> MarkedObject position -> MarkedObject position
markup _ mark (MarkedObject [] o) = MarkedObject [mark] o
markup _ mark (MarkedObject ms o) = MarkedObject (mark:ms) o 


createMark :: DUser -> MarkedObjectP p -> Mark p -> Either Error (MarkedObject p)
createMark duser@(DUser u _) mop@(MarkedObjectP mo _) mark = if accessible duser mop 
    then Right (markup u mark mo)
    else Left "no access"
