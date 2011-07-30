{-# LANGUAGE 
    TypeFamilies, 
    FlexibleContexts, 
    FlexibleInstances, 
    Rank2Types,
    DeriveDataTypeable, 
    ExistentialQuantification, 
    ScopedTypeVariables,
    BangPatterns
    #-}
module Data.Yteratee.Base where

import Control.Monad hiding(join)
import Control.Monad.Trans
import Data.Monoid 
import Data.Either
import Data.Data
import Data.Typeable()
import qualified Control.Exception as E
import Prelude as P hiding(head)

data IncrementalGetException = FailException String | EndOfStreamException | IgnoresEOS
    deriving (Show, Typeable)

instance E.Exception IncrementalGetException

-- FIXME: reimplement Stream as
-- data Stream s = Chunk s | EOS s
-- to build a correct Monoid on top of it
data Stream s = Chunk s | EOS (Maybe E.SomeException)

instance Monoid s => Monoid (Stream s) where
    mempty = Chunk mempty
    mappend (Chunk a) (Chunk b)  = Chunk (a`mappend`b)
    mappend (Chunk _) (EOS x)    = EOS x
    mappend (EOS x)   (Chunk _)  = EOS x
    mappend (EOS x)   (EOS _)    = EOS x

-- | Map a function over a stream.
instance Functor Stream where
    fmap f (Chunk xs) = Chunk $ f xs
    fmap _ (EOS x) = EOS x

-- | Request issued by iteratee to the upstream enumerator
-- data YRequest = YMoreData | YSeekAbs FileSize | YSeekRelative

data Yteratee s m a = Yteratee {runYter :: forall r . 
    ((Stream s)
     -> (a -> (Stream s) -> m r)
     -> (Yteratee s m a -> m r)
     -> (E.SomeException -> (Stream s) -> m r)
     -> m r) }

type YEnumerator s m a = Yteratee s m a -> m (Yteratee s m a)

-- | Enumeratee is the Enumerator which iterates some iteratee over iteratee
-- monad. Would we redefine YEnumerator as
type YEnumeratee sFrom sTo m a = 
    Yteratee sTo m a -> Yteratee sFrom m (Yteratee sTo m a)

instance (Monad m) => Monad (Yteratee s m) where
    return a = Yteratee $ \s done _ _ -> done a s
    fail msg = throwError (FailException msg)
    (>>=) = bindYteratee

instance (Functor m, Monad m) => Functor (Yteratee s m) where
  fmap f m = Yteratee $ \s done cont err ->
    let i_done a s' = done (f a) s'
        i_cont k = cont (fmap f k)
    in runYter m s i_done i_cont err

instance MonadTrans (Yteratee s) where
    lift m = Yteratee $ \s done _ _ -> m >>= flip done s

bindYteratee :: (Monad m) => Yteratee s m a -> (a -> Yteratee s m b) -> Yteratee s m b
bindYteratee ma f = Yteratee $ \s done cont err -> 
    let i_done a s' = runYter (f a) s' done cont err
        i_cont k = cont (bindYteratee k f)
    in runYter ma s i_done i_cont err

throwError :: (Monad m, E.Exception e) => e -> Yteratee s m a
throwError e = Yteratee $ \s _ _ err -> err (E.toException e) s

throwEOS :: (Monad m) => Yteratee s m a
throwEOS = throwError EndOfStreamException

identity :: (Monad m) => Yteratee s m ()
identity = Yteratee $ \s done _ _ -> done () s

runCheck :: (Monad m) => Yteratee s m a -> m (Either E.SomeException a)
runCheck g = runYter g (EOS Nothing) i_done i_cont i_err
    where i_done a _ = return (Right a)
          i_cont _ = return (Left (E.toException IgnoresEOS))
          i_err e _ = return (Left e)

run :: (Monad m) => Yteratee s m a -> m a
run i = runCheck i >>= either E.throw return

-- | Checks it's argument for error. Doesn't save and restore the stream. Use this function
-- instead of @try@ whenever possible.
checkErr :: (Monad m) => Yteratee s m a -> Yteratee s m (Either E.SomeException a)
checkErr p = Yteratee $ \s done cont _ ->
    let i_done a s' = done (Right a) s'
        i_err e s' = done (Left e) s'
        i_cont k = cont (checkErr k)
    in runYter p s i_done i_cont i_err

{-
-- | Same as @checkErr@ but saves stream state and rolls back in case of error
try :: (Monad m, Monoid s) => Yteratee s m a -> Yteratee s m (Either E.SomeException a)
try = step mempty
    where
        step save p = Yteratee $ \s done cont err ->
            case s of
                (Chunk new) ->
                    let cache = save`mappend`s
                        i_done a s' = done (Right a) s'
                        i_err e _ = done (Left e) cache
                        i_cont k = cont (step cache k)
                    in runYter p s i_done i_cont i_err
                eos -> 
-}

stream2stream :: (Monad m, Monoid s) => Yteratee s m s
stream2stream = step mempty 
    where 
        step save = Yteratee $ \s done cont _ ->
            case s of
                (Chunk new) -> cont (step $ save`mappend`new)
                eos -> done save eos

-- | Takes an iteratee and terminates it's stream with EOS. The iteratee has
-- to return a value or an error will be thrown.
join :: (Monad m) => Yteratee s m a -> Yteratee s m a
join g = Yteratee $ \s done _ err -> 
    let i_done a _ = done a s
        i_cont _ = err (E.toException IgnoresEOS) s
        i_err e _ = err e s
    in runYter g (EOS Nothing) i_done i_cont i_err

-- | Same as @join@ but iteratee passed in in monadic way
joinI :: (Monad m) => Yteratee s m (Yteratee s m a) -> Yteratee s m a
joinI = (>>= join)

-- | Feeds an enumerator with results of transforming iteratee
convStreamE :: (Monad m) => Yteratee s1 m s2 -> YEnumeratee s1 s2 m a
convStreamE t i = feedCallback t i $ \s i' ->
    let i_done a _ = return (True, return a)
        i_err  e _ = return (True, throwError e)
        i_cont i''   = return (False, i'')
    in runYter i' (Chunk s) i_done i_cont i_err

isStreamFinished :: (Monad m) => Yteratee s m Bool
isStreamFinished = Yteratee $ \s done _ _ ->
    case s of
        (Chunk _) -> done False s
        eos -> done True eos

-- | Use iteratee supplied to repeatedly feed the callback with data. Callback
-- may request stop by returning True. 
feedCallback :: (Monad m) 
    => Yteratee s1 m s2
    -> st 
    -> (s2 -> st -> m (Bool, st)) 
    -> Yteratee s1 m st
feedCallback g st f = do
    b <- isStreamFinished
    case b of
        True -> return st
        False -> do
            s2 <- g
            (stop, res) <- lift $ f s2 st
            case (stop, res) of
                (True, st') -> return st'
                (False, st') -> feedCallback g st' f

enumPureCheckDone :: (Monad m) => Stream s -> Yteratee s m a -> m (Maybe (Stream s), Yteratee s m a)
enumPureCheckDone s g = runYter g s i_done i_cont i_err
    where i_done a s' = return (Just s', return a)
          i_err e s' = return (Just s',throwError e)
          i_cont k = return (Nothing, k)

enumPure :: (Monad m) => Stream s -> Yteratee s m a -> m (Yteratee s m a)
enumPure s g = enumPureCheckDone s g >>= return . snd

enumPure1Chunk :: (Monad m) => s -> Yteratee s m a -> m (Yteratee s m a)
enumPure1Chunk s g = enumPure (Chunk s) g

enumFromCallback :: (Monad m) 
    => (st -> m (Either (Maybe E.SomeException) (st, s)))
    -> st
    -> YEnumerator s m a
enumFromCallback cb st i = do
    res <- cb st
    case res of
        Left e -> enumPure (EOS e) i
        Right (st',s) -> do
            (done,i') <- enumPureCheckDone (Chunk s) i
            case done of
                Just _ -> return i'
                Nothing -> enumFromCallback cb st' i'

