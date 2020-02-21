module Model 
    ( Object(..)
    , Attribute
    , Entity(..)
    , Mark(..)
    , VO(..)
    , User(..)
    , Username
    , Id
    ) where

type Bytes = String

data Object = Document String Bytes 
            | Image String Bytes
            | Dialog String [String]

data Attribute = Option String Bool
               | Case String [(String, String)]
               | TextLine String

data Entity = Entity String [Attribute]

data Mark p = Mark p Entity

data VO = VO Object

type Id = Int
type Username = String

data User = User { id       :: Id
                 , username :: String
                 , password :: String 
} deriving (Show, Read)