{-# LANGUAGE OverloadedStrings #-}
{-|
Storage module implementation.
TODO: use something, taht get ability for abstract concrete type of storage
-}
module UserStorage where

import qualified Data.ByteString as Str
import qualified Data.ByteString.UTF8 as Utf8
import Model
import Data.Text (splitOn, intercalate, isInfixOf, pack, unpack)


{-
   What i want?
   - Clean signature. No think about storage
   - less touch storage in business logic
-}

-- TODO: extract to evironment, configuration, etc
dbFilepath = "db.txt"

type GetUserF = Username -> IO (Maybe User)
type GetUsersF = IO [User]


-- | get all users in storage
getUsers :: IO [User]
getUsers = map read <$> readLines dbFilepath

-- | search user in storage
getUser :: Username -> IO (Maybe User)
getUser "" = pure Nothing
getUser name = do
  users <- getUsers
  let matched = filter (\x -> (username x) == name) users in
    if length matched == 0 
      then pure Nothing
      else pure $ Just (head matched)

-- | store user in storage
storeUser :: String -> String -> IO (User)
storeUser username pws = do
  users <- getUsers
  let newID = createId users in
    let usr = User newID username pws in
      (Str.appendFile dbFilepath $ Utf8.fromString $ (show usr) ++ "\n") >> pure usr

{-|
technical. 
used for getting lines from file of just strings, not bytestrings and etc.
-}
readLines :: FilePath -> IO [String]
readLines filename = do
  content <- Str.readFile filename
  pure $ (lines . Utf8.toString) content

{- | create new Id from userlist. Its just increment last 
user id from given user list
TODO: make its monadic and use monad as context.
In one case - just pass user userlist or last user id,
in other - io, random, state, etc. 
-} 
createId :: [User] -> Id
createId [] = 1
createId xs = (succ . Model.id . last) xs
