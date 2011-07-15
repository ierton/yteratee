{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Yteratee.Test where

import Data.Maybe
import Data.Yteratee as I
import Data.ListLike as LL
import Data.List as L
import Control.Monad
import Control.Monad.Identity
import Test.QuickCheck
import Prelude as P

instance Show (a -> b) where
  show _ = "<<function>>"

runner1 = runIdentity . I.run . runIdentity

enumSpecial xs n = enumPure1Chunk LL.empty >=> enumPureNChunk xs n

prop_list xs = runner1 (enumPure1Chunk xs stream2list) == xs
  where types = xs :: [Int]

prop_clist xs n = n > 0 ==> runner1 (enumSpecial xs n stream2list) == xs
  where types = xs :: [Int]

prop_takeWhile f xs = runner1 (enumPure1Chunk xs (I.takeWhile f)) == fst (L.span f xs)
  where types = xs :: [Int]

prop_takeWhile2 f xs = runner1 (enumPure1Chunk xs (I.takeWhile f >> I.stream2list)) == snd (L.span f xs)
  where types = xs :: [Int]

prop_takeWhileE f xs = runner1 (enumPure1Chunk xs (joinI $ I.takeWhileE f I.stream2stream)) == fst (L.span f xs)
  where types = xs :: [Int]

prop_takeWhileE2 f xs = runner1 (enumPure1Chunk xs (joinI (I.takeWhileE f I.stream2stream) >> I.stream2list)) == snd (L.span f xs)
  where types = xs :: [Int]

prop_head xs = P.length xs > 0 ==> runner1 (enumPure1Chunk xs I.head) == L.head xs
  where types = xs :: [Int]

prop_head2 xs = P.length xs > 0 ==> runner1 (enumPure1Chunk xs (I.head >> I.stream2list)) == L.tail xs
  where types = xs :: [Int]

