{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module BitBasket where

import Data.Maybe
import Data.Word
import Data.Bits
import Control.Monad

-- Right-to-Left bit basket
data BitBasket a = BitBasket Int a
    deriving(Eq,Show)

unsafeUnBasket :: (Bits a) => BitBasket a -> a
unsafeUnBasket (BitBasket 0 _) = 0
unsafeUnBasket (BitBasket p b) = 
    let x = bit (p-1) in b .&. (x.|.(x - 1))

unBasket :: (Bits a) => BitBasket a -> Maybe a
unBasket (BitBasket 0 _) = Nothing
unBasket bb@(BitBasket _ _) = Just $ unsafeUnBasket $ bb

mkBasket :: (Bits a) => a -> BitBasket a
mkBasket a = BitBasket (bitSize a) a

bbhead :: (Bits a) => BitBasket a -> a
bbhead (BitBasket 0 _) = error "empty BitBasket"
bbhead (BitBasket p b) = if testBit b (p-1) then 1 else 0

bbtail :: (Bits a) => BitBasket a -> BitBasket a
bbtail (BitBasket 0 _) = error "empty BitBasket"
bbtail (BitBasket p b) = BitBasket (p-1) b

bbtake :: (Bits a) => Int -> BitBasket a -> a
bbtake 0 _ = 0
bbtake p bs = (h `shiftR` p) .&. bbtake (p-1) t
    where (h,t) = (bbhead bs, bbtail bs)

bbunpack :: (Bits a) => BitBasket a -> [Bool]
bbunpack (BitBasket 0 _) = []
bbunpack (BitBasket p b) = (testBit b (p-1)) : (bbunpack $ BitBasket (p-1) b)

bbnull :: BitBasket a -> Bool
bbnull (BitBasket 0 _) = True
bbnull (BitBasket _ _) = False

bbempty :: (Bits a) => BitBasket a
bbempty = BitBasket 0 0

bblength :: (Bits a) => BitBasket a -> Int
bblength (BitBasket p b) = 0 `max` ((bitSize b) `min` p)

bbappend :: (Bits a) => BitBasket a -> BitBasket a -> BitBasket a
bbappend bb1@(BitBasket p1 b1) bb2@(BitBasket p2 b2)
    | (p1 + p2) > bitSize b1 = error "BitBasket: no space left in basket"
    | otherwise = BitBasket (p1 + p2) ((b1 `shiftL` p2) .|. (unsafeUnBasket bb2))

bbsplitAt :: (Bits a) => Int -> BitBasket a -> (BitBasket a, BitBasket a)
bbsplitAt n' (BitBasket p b) = (b1,b2)
    where 
        n = 0 `max` (n' `min` p)
        b1 = BitBasket n (b `shiftR`(p-n))
        b2 = BitBasket (p-n) b

bbparse :: (Bits a) => BitBasket a -> [Int] -> Maybe [a]
bbparse _ [] = Just []
bbparse bb@(BitBasket p b) (w:ws) = do
    when (w>p) (fail "Not enough bits")
    v <- unBasket (BitBasket w (b `shiftR`(p-w)))
    vs <- bbparse (BitBasket (p-w) b) ws
    return (v:vs)

getBits :: (Bits a, Monad m) => [Int] -> a -> m [a]
getBits ws v = case bbparse (mkBasket v) ws of
                    Nothing -> fail "bbparse failed"
                    Just l -> return l

