module Password where
import Control.Applicative
import Control.Monad
import Control.Monad.Trans 
import Data.Char


isValid :: String -> Bool
isValid s = length s >= 8
            && any isAlpha s
            && any isNumber s
            && any isPunctuation s



newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a)}


instance Monad m => Applicative (MaybeT m) where
    pure = return
    (<*>) = ap


instance Monad m => Functor (MaybeT m) where
    fmap = liftM


instance Monad m => Monad (MaybeT m) where
    return = MaybeT . pure . Just
    x >>= f = MaybeT $ do m' <- runMaybeT x -- x :: MaybeT m (Maybe a); runMaybeT x :: m (Maybe a); <- - extract (Maybe a) from m
                          case m' of
                            Nothing -> pure Nothing -- put back in m
                            (Just a) -> runMaybeT $ f a


instance Monad m => Alternative (MaybeT m) where
    empty = MaybeT $ pure Nothing
    x <|> y = MaybeT $ do mval <- runMaybeT x
                          case mval of 
                            Nothing -> runMaybeT y
                            Just _ -> pure mval


instance Monad m => MonadPlus (MaybeT m) where
    mzero = empty
    mplus = (<|>)


instance MonadTrans MaybeT where
    lift = MaybeT . liftM Just

{-  Naive solution
getPassphrase :: IO (Maybe String)
getPassphrase = do s <- getLine
                   if isValid s then return $ Just s
                   else return Nothing

askPassphrase :: IO ()
askPassphrase = do putStrLn "Insert your new passphrase:"
                   maybe_value <- getPassphrase
                   case maybe_value of
                       Just value -> putStrLn "Storing in database..."  -- do stuff
                       Nothing -> putStrLn "Passphrase invalid."
-}


getPassphrase :: MaybeT IO String
getPassphrase = do s <- lift getLine
                   guard (isValid s)
                   return s

askPassphrase :: MaybeT IO ()
askPassphrase = do lift $ putStrLn "Insert your password: "
                   value <- getPassphrase
                   lift $ putStrLn "Saving..."