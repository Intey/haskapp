module Model where


type Id = Int
type Username = String


data User = User { id       :: Id
                 , username :: String
                 , password :: String 
} deriving (Show, Read)

type Bytes = String

data Object = Doc | Dialog

newtype Attribute = Attribute String 

data Entity = Entity String [Attribute]

newtype Position a = Position a

data Mark position = Mark position Entity User

data MarkedObject position = MarkedObject [(Mark position)] Object

newtype Context = Context {
    user :: User 
    }

-- apped 
markup :: User -> Mark position -> MarkedObject position -> MarkedObject position
markup u mark (MarkedObject [] o) = MarkedObject [mark] o
markup u mark (MarkedObject ms o) = MarkedObject (mark:ms) o 


-- permission models by departments

newtype Permission = Permission String deriving (Show, Read)
data Group = Group String [Permission] deriving (Show, Read)
data Department = Department Id String


data MarkedObjectP p = MarkedObjectP (MarkedObject p) Department
data DUser = DUser User Department

accessible (DUser _ (Department i _)) (MarkedObjectP _ (Department i' _)) = i == i'
type Error = String
createMark :: DUser -> MarkedObjectP p -> Mark p -> Either Error (MarkedObject p)
createMark duser@(DUser u _) mop@(MarkedObjectP mo _) mark = if accessible duser mop 
    then Right (markup u mark mo)
    else Left "no access"