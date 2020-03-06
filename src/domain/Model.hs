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

data MarkedObject position = MarkedObject [Mark position] Object

newtype Context = Context {
    user :: User 
    }

-- apped 
markup :: User -> Object -> Entity -> position -> MarkedObject position -> MarkedObject position
markup u o e p (MarkedObject [] _) = MarkedObject [Mark p e u] o 
markup u o e p (MarkedObject ms _) = MarkedObject ((Mark p e u):ms) o 


-- permission models by departments

newtype Permission = Permission String deriving (Show, Read)
data Group = Group String [Permission] deriving (Show, Read)
data Department = Department Id String


data MarkedObjectP p = MarkedObjectP p Department
data DUser = DUser User Department

accessible (DUser u (Department i _)) (MarkedObjectP p (Department i' _)) = i == i'

markupUseCase :: DUser -> MarkedObjectP p -> MarkedObjectP p