{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.StreamLike where

import Data.ListLike as LL

import Data.Word
import Data.Bits
import Data.Functor
import Data.ByteString as B
import Control.Comonad

-- | Stream type. s is a stream like list or ByteString; c is a type of
-- stream's element; i is stream size type.
class StreamLike s c where
    head :: s -> c
    tail :: s -> s
--     take :: Int -> s -> s
--     drop :: Int -> s -> s
--     splitAt :: Int -> s -> (s,s)
--     null :: s -> Bool
--     empty :: s
--     length :: s -> Int
--     span :: (c->Bool) -> s -> (s,s)
--     toList :: s -> [c]

-- conv :: (Integral a, Integral b) => a -> b
-- conv = fromInteger . toInteger

data Container s c a = Container (a -> c -> a) s a

-- instance (LL.ListLike s c) => Functor (Container a s c) where
--     fmap f (Container a b) = Container a (f b)

-- class Listable s c where
--     foobar :: s -> c

-- instance (LL.ListLike s c) => Listable (Container x s) c where

instance (LL.ListLike s c) => StreamLike (Container s c a) c where
    head (Container f s x) = LL.head s
    tail (Container f s x) = Container f (LL.tail s) (f x (LL.head s))
--     take (Container a b) = LL.take
--     drop (Container a b) = LL.drop
--     splitAt (Container a b) = LL.splitAt
--     null (Container a b) = LL.null
--     empty (Container a b) = LL.empty
--     length (Container a b) = LL.length
--     span (Container a b) = LL.span
--     toList (Container a b) = LL.toList
    
-- instance StreamLike ByteString Word8 Int where
--     head = B.head
--     tail = B.tail
--     take = B.take
--     drop = B.drop
--     splitAt n = B.splitAt n
--     null = B.null
--     empty = B.empty
--     length = B.length
--     span = B.span
--     toList = B.unpack

