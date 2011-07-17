{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Yteratee.Test where

import Data.Maybe
import Data.Yteratee as I
import Data.ListLike as LL
import Data.List as L
import Data.Monoid
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


-- takeE
runner2 = runIdentity . run . runner1

prop_take xs n = n >= 0 ==>
                 runner2 (enumPure1Chunk xs $ I.takeE n stream2list)
                 == runner1 (enumPure1Chunk (P.take n xs) stream2list)
    where types = xs :: [Int]

prop_take2 xs n = n > 0 ==>
                  runner2 (enumPure1Chunk xs $ I.takeE n peek)
                  == runner1 (enumPure1Chunk (P.take n xs) peek)
    where types = xs :: [Int]

-- conv helpers
convId :: (LL.ListLike s el, Monad m) => Yteratee s m s
convId = Yteratee $ \s done cont err ->
    case s of
        s@(Chunk xs) | LL.null xs -> cont convId
                     | otherwise -> done xs (Chunk mempty)
        eos    -> done mempty eos

type I = Yteratee [Int] Identity [Int]

instance (Num a, Ord a, Arbitrary a, Monad m) => Arbitrary (Yteratee [a] m [a]) where
  arbitrary = do
    n <- suchThat arbitrary (>0)
    --ns <- arbitrary
    elements [
              I.drop n >> stream2list
              --, I.break (< 5)
              ,I.peek >> stream2list
              -- ,I.heads ns >> stream2list
              ]

instance (Show a, LL.ListLike s el) => Show (Yteratee s Identity a) where
  show = (++) "<<Iteratee>> " . show . runIdentity . run

prop_convId xs = runner1 (enumPure1Chunk xs convId) == xs
  where types = xs :: [Int]

-- convStreamE
prop_convstream xs i = P.length xs > 0 ==>
                       runner2 (enumPure1Chunk xs $ convStreamE convId i)
                       == runner1 (enumPure1Chunk xs i)
  where types = (xs :: [Int], i :: I)

prop_convstream2 xs = P.length xs > 0 ==>
                      runner2 (enumPure1Chunk xs $ convStreamE convId I.head)
                      == runner1 (enumPure1Chunk xs I.head)
  where types = xs :: [Int]

prop_convstream3 xs = P.length xs > 0 ==>
                      runner2 (enumPure1Chunk xs $ convStreamE convId stream2list)
                      == runner1 (enumPure1Chunk xs stream2list)
  where types = xs :: [Int]

