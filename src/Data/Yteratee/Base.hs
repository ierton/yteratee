{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, Rank2Types,
    DeriveDataTypeable, ExistentialQuantification, ScopedTypeVariables #-}
module Data.Yteratee.Base where

import Control.Monad hiding(join)
import Control.Monad.Trans
import Control.Applicative
import qualified Control.Exception as E
import Data.Monoid 
import Data.Either
import Data.Data
import Data.Typeable
import qualified Data.ListLike as LL
import qualified Data.List as L
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
    mappend (Chunk a) (EOS x)    = EOS x
    mappend (EOS x)   (Chunk a)  = EOS x
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
    fail msg = throwError (E.toException $ FailException msg)
    (>>=) = bindYteratee

instance (Functor m, Monad m) => Functor (Yteratee s m) where
  fmap f m = Yteratee $ \s done cont err ->
    let i_done a s = done (f a) s 
        i_cont k = cont (fmap f k)
    in runYter m s i_done i_cont err

instance MonadTrans (Yteratee s) where
    lift m = Yteratee $ \s done _ _ -> m >>= flip done s

bindYteratee :: (Monad m) => Yteratee s m a -> (a -> Yteratee s m b) -> Yteratee s m b
bindYteratee ma f = Yteratee $ \s done cont err -> 
    let i_done a s' = runYter (f a) s' done cont err
        i_cont k = cont (bindYteratee k f)
    in runYter ma s i_done i_cont err

throwError e = Yteratee $ \s _ _ err -> err e s

throwEOS = throwError (E.toException EndOfStreamException)

identity = Yteratee $ \s done _ _ -> done undefined s

run :: (Monad m) => Yteratee s m a -> m a
run g = runYter g (EOS Nothing) i_done i_cont i_err
    where i_done a _ = return a
          i_cont _ = E.throw IgnoresEOS
          i_err e _ = E.throw e

{-
runCheck :: (Monad m) => Stream s -> Yteratee s m a -> m (Either (Yteratee s m a) (Either E.SomeException a,Stream s))
runCheck s g = runYter g s i_done i_cont i_err
    where i_done a s' = return (Right ((Right a),s'))
          i_cont k = return (Left k)
          i_err e s' = return (Right ((Left e),s'))
-}

-- | Checks it's argument for error. Doesn't save and restore the stream. Use this function
-- instead of @try@ whenever possible.
checkErr :: (Monad m) => Yteratee s m a -> Yteratee s m (Either E.SomeException a)
checkErr p = Yteratee $ \s done cont err ->
    let i_done a s' = done (Right a) s'
        i_err e s' = done (Left e) s'
        i_cont k = cont (checkErr k)
    in runYter p s i_done i_cont i_err

-- | Same as @checkErr@ but saves stream state and rolls back in case of error
try :: (Monad m) => Yteratee s m a -> Yteratee s m (Either E.SomeException a)
try p = Yteratee $ \s done cont err ->
    let i_done a s' = done (Right a) s'
        i_err e _ = done (Left e) s
        i_cont k = cont (try k)
    in runYter p s i_done i_cont i_err

stream2stream :: (Monad m, Monoid s) => Yteratee s m s
stream2stream = step mempty 
    where 
        step save = Yteratee $ \s done cont err ->
            case s of
                (Chunk new) -> cont (step $ save`mappend`new)
                eos -> done save eos

-- | Takes an iteratee and terminates it's stream with EOS. The iteratee has
-- to return a value or an error will be thrown.
join :: (Monad m) => Yteratee s m a -> Yteratee s m a
join g = Yteratee $ \s done cont err -> 
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
        s@(Chunk _) -> done False s
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

enumPureCheckDone :: (Monad m) => Stream s -> Yteratee s m a -> m (Bool, Yteratee s m a)
enumPureCheckDone s g = runYter g s i_done i_cont i_err
    where i_done a _ = return (True, return a)
          i_cont k = return (False,k)
          i_err e _ = return (True,throwError e)

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
                True -> return i'
                False -> enumFromCallback cb st' i'

