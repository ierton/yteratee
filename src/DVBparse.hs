{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, FlexibleContexts, BangPatterns #-}

import Data.Typeable
import Data.Data
import Data.Yteratee as I
import Data.Yteratee.IO
import Data.Map as M
import Data.ListLike
import Data.ByteString
import Data.Char
import Data.Word
import Data.String
import Control.Monad
import Control.Monad.Error
import Control.Exception
import Data.ByteString as BS

import System.Environment
import System.Exit

import BitBasket


startbyte = 0x47 :: Word8
psize = 188
pdeep = 3

conv :: (Integral a, Integral b) => a -> b
conv = fromInteger . toInteger

data TSPacket = TSPacket {
    tsp_pid :: Word16,
    tsp_afield :: Maybe AField,
    tsp_body :: ByteString,
    garbage :: Word32
    } deriving(Show)

data AField = AField {
    af_body :: BS.ByteString
    } deriving(Show)

-- instance Show TSPacket where
--     show p = "pid: " ++ (show $ tsp_pid p)


data EBadNews = ENotPacket | EInvalidAField | EBadLen
    deriving(Show,Typeable)

instance Error EBadNews
instance Exception EBadNews

getWord16 :: (Monad m) => Yteratee BS.ByteString m Word16
getWord16 = lift $ I.endianRead2 MSB

getWord8 :: (Monad m) => Yteratee BS.ByteString m Word8
getWord8 = lift $ I.head

getByteString :: (Monad m) => Word32 -> Yteratee BS.ByteString m BS.ByteString
getByteString = I.take

countBytes :: (Monad m) => Yteratee BS.ByteString m a -> Iteratee BS.ByteString m (a, Word32)
countBytes i = I.enumWith i I.length

countBytesE :: (Monad m) => Yteratee BS.ByteString m a -> IterateeE BS.ByteString m (a, Word32)
countBytesE i = do
    (res, sz) <- lift $ countBytes (runErrorT i)
    case res of 
        Left e -> throwError e
        Right a -> return (a,sz)

afield :: (MonadIO m) => IterateeE BS.ByteString m AField
afield = do
    len <- getWord8 
    when (((conv len) > (psize-4)) || ((conv len) < 0)) $
        throwError EInvalidAField
    body <- getByteString (conv len)
    return $ AField body

packet :: (MonadIO m) => IterateeE BS.ByteString m TSPacket
packet = do
    (magic,tmp1) <- countBytesE getWord8
    when (magic /= startbyte) (throwError ENotPacket)
    (tei:pusi:tp:pid:[]) <- getWord16 >>= getBits [1,1,1,13]
    (tsc:afc1:afc2:cc:[]) <- getWord8 >>= getBits [2,1,1,4]
    (afld,alen) <- countBytesE $ do
        case afc1 == 1 of
            True -> afield >>= return . Just
            False -> return Nothing
    let blen = (psize-4-alen)
    body <- case afc2 == 1 of
        True -> getByteString blen
        False -> getByteString blen >> return BS.empty
    return $ TSPacket pid afld body 0

-- | Searches for valid TS stream. Stream is valid if there are at least 1 valid
-- packet pending.
skipGarbage :: (Monad m) => Iteratee BS.ByteString m ()
skipGarbage = I.dropWhile (/= startbyte)

-- | Finds and parses one TS packet
packetF :: (MonadIO m) => Iteratee BS.ByteString m TSPacket
packetF = step 0
    where 
        step g = do
            (mpacket,ps) <- countBytes $ runErrorT $ packet
            case mpacket of
                Right p -> do
                    return p{garbage=g}
                Left e -> do
                    skip g
        skip g = do
            (_,g') <- countBytes $ skipGarbage
            step (g+g')

packets :: (MonadIO m) => Iteratee BS.ByteString m [TSPacket]
packets = do
    p <- checkErr packetF
    case p of
        Right p -> do
            ps <- packets
            return (p:ps)
        Left _ -> return []

data Statistics = Statistics {
    statPids :: !(M.Map Word16 Int),
    statAlen :: !(M.Map Int Int),
    pcount :: !Int
    } deriving(Show)

emptyStat = Statistics M.empty M.empty 0

enstat :: Statistics -> TSPacket -> Statistics
enstat s@(Statistics mp ma cnt) p = s{pcount=(cnt+1)}

stat :: (MonadIO m) => Iteratee [TSPacket] m Statistics
stat = I.foldl' enstat emptyStat 

iall :: (MonadIO m) => Iteratee BS.ByteString m Statistics
iall = I.joinI $ I.convStream packets stat

-- main = do
--     (f:_) <- getArgs
--     s <- I.fileDriver iall f
--     Prelude.putStrLn $ show s

main = do
    (f:_) <- getArgs
    p <- I.fileDriverVBuf 1024 (packetF) f
    Prelude.putStrLn $ show p

-- main = do
--     (f:_) <- getArgs
--     p <- I.fileDriverVBuf 1024 (packetF) f
--     Prelude.putStrLn $ show p

-- main = do
--     (f:_) <- getArgs
--     p <- I.fileDriver (countBytes (skipGarbage>>I.head>>skipGarbage)) f
--     Prelude.putStrLn $ show p

