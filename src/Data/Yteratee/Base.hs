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

data Stream s = Chunk s | EOS

instance Monoid s => Monoid (Stream s) where
    mempty = Chunk mempty
    mappend (Chunk a) (Chunk b)  = Chunk (a`mappend`b)
    mappend (Chunk a) (EOS)      = EOS
    mappend (EOS)     (Chunk a)  = EOS
    mappend (EOS)     (EOS)      = EOS

-- | Map a function over a stream.
instance Functor Stream where
    fmap f (Chunk xs) = Chunk $ f xs
    fmap _ (EOS) = EOS

data Yteratee s m a = Yteratee {runYter :: forall r . 
    ((Stream s)
     -> (a -> (Stream s) -> m r)
     -> (Yteratee s m a -> m r)
     -> (E.SomeException -> (Stream s) -> m r)
     -> m r) }

type YEnumeratee sFrom sTo m a = 
    Yteratee sTo m a -> Yteratee sFrom m (Yteratee sTo m a)

instance (Monad m) => Monad (Yteratee s m) where
    return a = Yteratee $ \s done _ _ -> done a s
    fail msg = throwError (E.toException $ FailException msg)
    (>>=) = bindYteratee

instance (Functor m, Monad m) => Functor (Yteratee s m) where
  fmap f m = Yteratee $ \s done cont err ->
    let i_done a s = done (f a) s 
        i_cont = cont . fmap f
    in runYter m s i_done i_cont err

bindYteratee :: (Monad m) => Yteratee s m a -> (a -> Yteratee s m b) -> Yteratee s m b
bindYteratee ma f = Yteratee $ \s done cont err -> 
    let i_done a s' = runYter (f a) s' done cont err
        i_cont k = cont $ bindYteratee k f
    in runYter ma s i_done i_cont err

throwError e = Yteratee $ \s _ _ err -> err e s

throwEOS = throwError (E.toException EndOfStreamException)

-- | Checks it's argument for error. Doesn't save and restore the stream. Use this function
-- instead of @try@ whenever possible.
checkErr :: (Monad m) => Yteratee s m a -> Yteratee s m (Either E.SomeException a)
checkErr p = Yteratee $ \s done cont err ->
    let i_done a s' = done (Right a) s'
        i_err e s' = done (Left e) s'
        i_cont = cont . checkErr
    in runYter p s i_done i_cont i_err

-- | Same as @checkErr@ but consumes input only if argument succeeds
try :: (Monad m) => Yteratee s m a -> Yteratee s m (Either E.SomeException a)
try p = Yteratee $ \s done cont err ->
    let i_done a s' = done (Right a) s'
        i_err e _ = done (Left e) s
        i_cont = cont . try
    in runYter p s i_done i_cont i_err

stream2stream :: (Monad m, Monoid s) => Yteratee s m s
stream2stream = step mempty 
    where 
        step save = Yteratee $ \s done cont err ->
            case s of
                (Chunk new) -> cont (step $ save`mappend`new)
                (EOS) -> done save EOS

join :: (Monad m) => Yteratee s m a -> Yteratee s m a
join g = Yteratee $ \s done cont err -> 
    let i_done a _ = done a s
        i_cont _ = err (E.toException IgnoresEOS) s
        i_err e _ = err e s
    in runYter g EOS i_done i_cont i_err

joinI :: (Monad m) => Yteratee s m (Yteratee s m a) -> Yteratee s m a
joinI = (>>= join)

run :: (Monad m) => Yteratee s m a -> m a
run g = runYter g EOS i_done i_cont i_err
    where i_done a _ = return a
          i_cont _ = E.throw IgnoresEOS
          i_err e _ = E.throw e

identity = Yteratee $ \s done _ _ -> done undefined s

enumPure1Chunk :: (Monad m) => s -> Yteratee s m a -> m (Yteratee s m a)
enumPure1Chunk s g =
    let i_done a _ = return (return a)
        i_cont k = return k
        i_err e _ = return (throwError e)
    in runYter g (Chunk s) i_done i_cont i_err



