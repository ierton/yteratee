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
psize = 188 :: Word32
pdeep = 3

conv :: (Integral a, Integral b) => a -> b
conv = fromInteger . toInteger

toInt :: (Integral a) => a -> Int
toInt = conv

toWord32 :: (Integral a) => a -> Word32
toWord32 = conv

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
getWord16 = I.endianRead2 MSB

getWord8 :: (Monad m) => Yteratee BS.ByteString m Word8
getWord8 = I.head

getByteString :: (Monad m) => Word32 -> Yteratee BS.ByteString m BS.ByteString
getByteString l = I.take (toInt l)

countBytes :: (Monad m) => Yteratee BS.ByteString m a -> Yteratee BS.ByteString m (a, Word32)
countBytes i = I.enumWith i (I.length >>= return . conv)

raiseError = I.throwError . toException

afield :: (MonadIO m) => Yteratee BS.ByteString m AField
afield = do
    len <- getWord8 >>= return . toWord32
    when (len > (psize-4)) $
        raiseError EInvalidAField
    body <- getByteString len
    return $ AField body

packet :: (MonadIO m) => Yteratee BS.ByteString m TSPacket
packet = do
    (magic,tmp1) <- countBytes getWord8
    when (magic /= startbyte) (raiseError ENotPacket)
    (tei:pusi:tp:pid:[]) <- getWord16 >>= getBits [1,1,1,13]
    (tsc:afc1:afc2:cc:[]) <- getWord8 >>= getBits [2,1,1,4]
    (afld,alen) <- countBytes $ do
        case afc1 == 1 of
            True -> afield >>= return . Just
            False -> return Nothing
    let blen = toWord32 (psize-4-alen)
    body <- case afc2 == 1 of
        True -> getByteString blen
        False -> getByteString blen >> return BS.empty
    return $ TSPacket pid afld body 0

-- | Searches for valid TS stream. Stream is valid if there are at least 1 valid
-- packet pending.
skipGarbage :: (Monad m) => Yteratee BS.ByteString m ()
skipGarbage = I.takeWhile (/= startbyte) >> return ()

{-
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

-}

