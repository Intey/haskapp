module Lib where

import qualified Model as Domain
import qualified UserStorage as US

{-| Gets object for markup.
Go to storage, select free object. Assign it to current user, 
create VO and return it

-}
getObject :: IO Domain.Object
getObject = pure $ Domain.Dialog "first dialog" ["message 1", "message 2"]
