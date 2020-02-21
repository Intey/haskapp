module Logger where

type Log = [String]

newtype Logger a = Logger { execLogger :: (a, Log) }

runLogger :: Logger a -> (a, Log)
runLogger = execLogger

instance Functor Logger where
    fmap f (Logger (a, l)) = Logger (f a, (l ++ pure "fmapped"))

instance Applicative Logger where
    pure a = Logger (a, [])
    (Logger (f, l)) <*> (Logger (x, ll)) = Logger ((f x), l ++ ll)


instance Monad Logger where
    return a = Logger (a, [])
    m >>= k = let (v1, l1) = execLogger m
                  n      = k v1
                  (v2, l2) = execLogger n
              in Logger (v2, l1 ++ l2)

record :: String -> Logger ()
record s = Logger ((), [s])


liftM :: (Monad m) => (a -> b) -> (m a) -> (m b)
liftM f m = m >>= \i -> return (f i)


type SimpleState s a = s -> (a, s)
