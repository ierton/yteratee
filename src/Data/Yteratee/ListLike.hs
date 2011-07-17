-- {-# LANGUAGE UndecidableInstances #-}
-- {-# Rank2Types #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, Rank2Types,
    DeriveDataTypeable, ExistentialQuantification, ScopedTypeVariables #-}
module Data.Yteratee.ListLike where

import Data.Yteratee.Base
import Control.Monad hiding(join)
import Control.Monad.Trans
import Control.Applicative
import Control.Exception as E
import Data.Monoid 
import Data.Either
import Data.Data
import Data.Typeable
import qualified Data.ListLike as LL
import qualified Data.List as L
import Prelude as P hiding(head)

head :: (LL.ListLike s el, Monad m) => Yteratee s m el
head = Yteratee step
    where 
        step (Chunk s) done cont err = 
            case LL.null s of
                True -> cont head
                False -> done (LL.head s) (Chunk $ LL.tail s)
        step eos _ _ err = err (toException EndOfStreamException) eos

stream2list :: (Monad m, LL.ListLike s el) => Yteratee s m [el]
stream2list = step []
    where 
        step save = Yteratee $ \s done cont err ->
            case s of
                (Chunk new) -> cont (step $ save ++ (LL.toList new))
                eos -> done save eos

takeE :: (LL.ListLike s el, Monad m) => Int -> YEnumeratee s s m a
takeE 0 g = (return g)
takeE n g = Yteratee $ \stream done cont err ->
    case stream of
        c@(Chunk s) -> 
            let len = LL.length s
            in case len >= n of
               True -> let (h,t) = (LL.take n s, LL.drop n s)
                           i_done a _ = done (return a) (Chunk t)
                           i_cont k = done k (Chunk t)
                           i_err e _ = done (throwError e) (Chunk t)
                       in runYter g (Chunk h) i_done i_cont i_err
               False -> let
                           i_done a s' = cont $ takeE (n-len) (return a)
                           i_cont k = cont $ takeE (n-len) k
                           i_err e s' = cont $ takeE (n-len) (throwError e)
                       in runYter g c i_done i_cont i_err
        eos -> done g eos

take n = takeE n stream2stream >>= join

takeWhileE :: (Monad m, LL.ListLike s el) => (el -> Bool) -> YEnumeratee s s m a
takeWhileE pred g = Yteratee $ \s done cont err ->
    case s of
        Chunk s ->
            let (h,t) = LL.span pred s
            in case (LL.null t) of
               False -> 
                    let i_done a _ = done (return a) (Chunk t)
                        i_err e _ = done (throwError e) (Chunk t)
                        i_cont k = done k (Chunk t) 
                    in runYter g (Chunk h) i_done i_cont i_err
               True ->
                    let i_done a _ = cont $ takeWhileE pred (return a)
                        i_err e _ = cont $ takeWhileE pred (throwError e)
                        i_cont k = cont $ takeWhileE pred k 
                    in runYter g (Chunk h) i_done i_cont i_err
        eos -> done g eos

takeWhile :: (Monad m, LL.ListLike s el) => (el -> Bool) -> Yteratee s m s
takeWhile pred = takeWhileE pred stream2stream >>= join

enumPureNChunk :: (Monad m, LL.ListLike s el) => s -> Int -> Yteratee s m a -> m (Yteratee s m a)
enumPureNChunk s n g = L.foldl' (>>=) (return g) $ P.map enumPure1Chunk $ breakN (chunklen n) s
    where
        chunklen x | (x <= 0) || (x > (LL.length s)) = 1
                   | otherwise = (LL.length s)`div`x
        breakN n l | LL.null l = []
                   | otherwise = (LL.take n l) : (breakN n (LL.drop n l))

peek :: (Monad m, LL.ListLike s el) => Yteratee s m (Maybe el)
peek = Yteratee $ \s done cont err ->
    case s of
        Chunk s | LL.null s -> cont peek
                | otherwise -> done (Just $ LL.head s) (Chunk $ LL.tail s)
        eos -> done Nothing eos

drop :: (Monad m, LL.ListLike s el) => Int -> Yteratee s m ()
drop n = Yteratee $ \s done cont err ->
    case s of
        Chunk c -> 
            case LL.length c >= n of
                True -> done () (Chunk $ LL.drop n c)
                False -> cont $ Data.Yteratee.ListLike.drop (n - LL.length c)
        eos -> done () eos

length :: (Monad m, LL.ListLike s el, Num a) => Yteratee s m a
length = step (conv 0)
    where 
        conv = fromInteger . toInteger
        step n = Yteratee $ \s done cont err -> case s of
                    Chunk c -> cont $ step (n + (conv $ LL.length c))
                    eos -> done n eos
{-
consumed :: (Monad m, Monoid s, LL.ListLike s el) => Yteratee s m a -> Yteratee s m (s,a)
consumed i = step mempty
    where 
        step = Yteratee $ \s done cont err ->
            let consumed (Chunk a) (Chunk b) = Chunk $ LL.take ((LL.length b) - (LL.length a)) b
                i_done a s' = return (consumed s' s, a)
                i_cont k = 
                i_err e s' = 
            in runYter i s i_done i_cont i_err
-}

