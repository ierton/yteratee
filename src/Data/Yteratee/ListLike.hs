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
module Data.Yteratee.ListLike where

import Data.Yteratee.Base
import Control.Monad hiding(join)
import Control.Exception as E
import Data.Either
import qualified Data.ListLike as LL
import qualified Data.List as L
import Prelude as P hiding(head,pred)

head :: (LL.ListLike s el, Monad m) => Yteratee s m el
head = Yteratee step where
    step :: (LL.ListLike s el, Monad m) => forall r .
        (Stream s)
         -> (el -> (Stream s) -> m r)
         -> (Yteratee s m el -> m r)
         -> (E.SomeException -> (Stream s) -> m r)
         -> m r
    step (Chunk s) done cont _ = 
        case LL.null s of
            True -> cont head
            False -> done (LL.head s) (Chunk $ LL.tail s)
    step eos _ _ err = err (toException EndOfStreamException) eos

stream2list :: (Monad m, LL.ListLike s el) => Yteratee s m [el]
stream2list = step []
    where 
        step save = Yteratee $ \s done cont _ ->
            case s of
                (Chunk new) -> cont (step $ save ++ (LL.toList new))
                eos -> done save eos

takeE :: (LL.ListLike s el, Monad m) => Int -> YEnumeratee s s m a
takeE 0 g = (return g)
takeE n g = Yteratee $ \stream done cont _ ->
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
                           i_done a _ = cont $ takeE (n-len) (return a)
                           i_cont k = cont $ takeE (n-len) k
                           i_err e _ = cont $ takeE (n-len) (throwError e)
                       in runYter g c i_done i_cont i_err
        eos -> done g eos

take :: (LL.ListLike s el, Monad m) => Int -> Yteratee s m s
take n = takeE n stream2stream >>= join

takeWhileE :: (Monad m, LL.ListLike s el) => (el -> Bool) -> YEnumeratee s s m a
takeWhileE pr g = Yteratee $ \s done cont _ ->
    case s of
        Chunk x ->
            let (h,t) = LL.span pr x
            in case (LL.null t) of
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

takeWhile :: (Monad m, LL.ListLike s el) => (el -> Bool) -> Yteratee s m s
takeWhile pred = takeWhileE pred stream2stream >>= join

enumPureNChunk :: (Monad m, LL.ListLike s el) => s -> Int -> Yteratee s m a -> m (Yteratee s m a)
enumPureNChunk s n g = L.foldl' (>>=) (return g) $ P.map enumPure1Chunk $ breakN (chunklen n) s
    where
        chunklen x | (x <= 0) || (x > (LL.length s)) = 1
                   | otherwise = (LL.length s)`div`x
        breakN m l | LL.null l = []
                   | otherwise = (LL.take m l) : (breakN m (LL.drop m l))

peek :: (Monad m, LL.ListLike s el) => Yteratee s m (Maybe el)
peek = Yteratee $ \s done cont _ ->
    case s of
        Chunk x | LL.null x -> cont peek
                | otherwise -> done (Just $ LL.head x) (Chunk $ LL.tail x)
        eos -> done Nothing eos

drop :: (Monad m, LL.ListLike s el) => Int -> Yteratee s m ()
drop n = Yteratee $ \s done cont _ ->
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
        step !n = Yteratee $ \s done cont _ -> case s of
                    Chunk c -> cont $ step $! (n + (conv $ LL.length c))
                    eos -> done n eos

enumWith
  :: (Monad m, LL.ListLike s el)
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
        cut (Chunk a) (Chunk b) = Chunk $ LL.take ((LL.length a) - (LL.length b)) a
        cut x@_        _        = x

