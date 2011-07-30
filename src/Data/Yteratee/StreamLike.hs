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
module Data.Yteratee.StreamLike where

import Data.Yteratee.Base
import Control.Monad hiding(join)
import Control.Exception as E
import Data.Either
import qualified Data.StreamLike as SL
import qualified Data.List as L
import Prelude as P hiding(head,pred)

head :: (Monad m, SL.StreamLike s el) => Yteratee s m el
head = Yteratee step
    where 
        step (Chunk s) done cont _ = 
            case SL.null s of
                True -> cont head
                False -> done (SL.head s) (Chunk $ SL.tail s)
        step eos _ _ err = err (toException EndOfStreamException) eos

stream2list :: (Monad m, SL.StreamLike s el) => Yteratee s m [el]
stream2list = step []
    where 
        step save = Yteratee $ \s done cont _ ->
            case s of
                (Chunk new) -> cont (step $ save ++ (SL.toList new))
                eos -> done save eos

takeE :: (Monad m, SL.StreamLike s el) => Int -> YEnumeratee s s m a
takeE 0 g = (return g)
takeE n g = Yteratee $ \stream done cont _ ->
    case stream of
        c@(Chunk s) -> 
            let len = SL.length s
            in case len >= n of
               True -> let (h,t) = SL.splitAt n s
                           i_done a _ = done (return a) (Chunk t)
                           i_cont k = done k (Chunk t)
                           i_err e _ = done (throwError e) (Chunk t)
                       in runYter g (Chunk h) i_done i_cont i_err
               False -> let
                           i_done a _ = cont $ takeE (n-len) (return a)
                           i_cont k = cont $ takeE (n-len) k
                           i_err e _ = cont $ takeE (n-len) (throwError e)
                       in runYter g c i_done i_cont i_err
        eos -> done g eos

take :: (Monad m, SL.StreamLike s el) => Int -> Yteratee s m s
take n = takeE n stream2stream >>= join

takeWhileE :: (Monad m, SL.StreamLike s el) => (el -> Bool) -> YEnumeratee s s m a
takeWhileE pr g = Yteratee $ \s done cont _ ->
    case s of
        Chunk x ->
            let (h,t) = SL.span pr x
            in case (SL.null t) of
               False -> 
                    let i_done a _ = done (return a) (Chunk t)
                        i_err e _ = done (throwError e) (Chunk t)
                        i_cont k = done k (Chunk t) 
                    in runYter g (Chunk h) i_done i_cont i_err
               True ->
                    let i_done a _ = cont $ takeWhileE pr (return a)
                        i_err e _ = cont $ takeWhileE pr (throwError e)
                        i_cont k = cont $ takeWhileE pr k 
                    in runYter g (Chunk h) i_done i_cont i_err
        eos -> done g eos

takeWhile :: (Monad m, SL.StreamLike s el) => (el -> Bool) -> Yteratee s m s
takeWhile pred = takeWhileE pred stream2stream >>= join

enumPureNChunk :: (Monad m, SL.StreamLike s el) => s -> Int -> Yteratee s m a -> m (Yteratee s m a)
enumPureNChunk s n g = L.foldl' (>>=) (return g) $ P.map enumPure1Chunk $ breakN (chunklen n) s
    where
        chunklen x | (x <= 0) || (x > (SL.length s)) = 1
                   | otherwise = (SL.length s)`div`x
        breakN m l | SL.null l = []
                   | otherwise = let (h,t) = SL.splitAt m l in h : (breakN m t)

peek :: (Monad m, SL.StreamLike s el) => Yteratee s m (Maybe el)
peek = Yteratee $ \s done cont _ ->
    case s of
        Chunk x | SL.null x -> cont peek
                | otherwise -> done (Just $ SL.head x) (Chunk $ SL.tail x)
        eos -> done Nothing eos

drop :: (Monad m, SL.StreamLike s el) => Int -> Yteratee s m ()
drop n = Yteratee $ \s done cont _ ->
    case s of
        Chunk c -> 
            case SL.length c >= n of
                True -> done () (Chunk $ SL.drop n c)
                False -> cont $ Data.Yteratee.StreamLike.drop (n - SL.length c)
        eos -> done () eos

length :: (Monad m, SL.StreamLike s el, Num a) => Yteratee s m a
length = step (conv 0)
    where 
        conv = fromInteger . toInteger
        step !n = Yteratee $ \s done cont _ -> case s of
                    Chunk c -> cont $ step $! (n + (conv $ SL.length c))
                    eos -> done n eos

enumWith
  :: (Monad m, SL.StreamLike s el)
  => Yteratee s m a
  -> Yteratee s m b
  -> Yteratee s m (a, b)
enumWith i1 i2 = Yteratee $ \s done cont err -> do
    (stop, i1') <- enumPureCheckDone s i1
    case stop of
        Just s' -> do
            i2' <- enumPure (cut s s') i2
            ma <- runCheck i1'
            mb <- runCheck i2'
            case (ma,mb) of
                (Left e ,_) -> err e s'
                (_ ,Left e) -> err e s'
                (Right a,Right b) -> done (a,b) s'
        Nothing -> do
            i2' <- enumPure s i2
            cont $ enumWith i1' i2'
    where
        cut (Chunk a) (Chunk b) = Chunk $ SL.take ((SL.length a) - (SL.length b)) a
        cut x@_        _        = x

