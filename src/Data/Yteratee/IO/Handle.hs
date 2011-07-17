{-# LANGUAGE ScopedTypeVariables #-}

-- |Random and Binary IO with generic Iteratees.  These functions use Handles
-- for IO operations, and are provided for compatibility.  When available,
-- the File Descriptor based functions are preferred as these wastefully
-- allocate memory rather than running in constant space.

module Data.Yteratee.IO.Handle(
  enumHandle
  ,enumFile
  ,fileDriverHandle
)

where

import Data.Yteratee.IO.ReadableChunk
import Data.Yteratee.Base

import Control.Exception
import Control.Monad
import Control.Monad.CatchIO as CIO
import Control.Monad.IO.Class

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

import System.IO


-- ------------------------------------------------------------------------
-- Binary Random IO enumerators

makeHandleCallback ::
  (MonadCatchIO m, ReadableChunk s el) =>
  Ptr el
  -> Int
  -> Handle
  -> (st -> m (Either (Maybe SomeException) (st, s)))
makeHandleCallback p bsize h st = do
  n' <- liftIO (CIO.try $ hGetBuf h p bsize :: IO (Either SomeException Int))
  case n' of
    Left e -> return $ Left $ Just e
    Right 0 -> return $ Left $ Nothing
    Right n -> liftM (\s -> Right (st,s)) $ readFromPtr p (fromIntegral n)

-- |The (monadic) enumerator of a file Handle.  This version enumerates
-- over the entire contents of a file, in order, unless stopped by
-- the iteratee.  In particular, seeking is not supported.
-- Data is read into a buffer of the specified size.
enumHandle ::
 forall s el m a.(ReadableChunk s el, MonadCatchIO m) =>
  Int -- ^Buffer size (number of elements per read)
  -> Handle
  -> YEnumerator s m a
enumHandle bs h i =
  let bufsize = bs * sizeOf (undefined :: el)
  in CIO.bracket (liftIO $ mallocBytes bufsize)
                 (liftIO . free)
                 (\p -> enumFromCallback (makeHandleCallback p bufsize h) () i)

-- ----------------------------------------------
-- File Driver wrapper functions.

enumFile' :: (MonadCatchIO m, ReadableChunk s el) =>
  (Int -> Handle -> YEnumerator s m a)
  -> Int -- ^Buffer size
  -> FilePath
  -> YEnumerator s m a
enumFile' enumf bufsize filepath iter = CIO.bracket
  (liftIO $ openBinaryFile filepath ReadMode)
  (liftIO . hClose)
  (flip (enumf bufsize) iter)

enumFile ::
  (MonadCatchIO m, ReadableChunk s el)
  => Int                 -- ^Buffer size
  -> FilePath
  -> YEnumerator s m a
enumFile = enumFile' enumHandle

-- |Process a file using the given @Iteratee@.  This function wraps
-- @enumHandle@ as a convenience.
fileDriverHandle
  :: (MonadCatchIO m, ReadableChunk s el) =>
     Int                      -- ^Buffer size (number of elements)
     -> Yteratee s m a
     -> FilePath
     -> m a
fileDriverHandle bufsize iter filepath =
  enumFile bufsize filepath iter >>= run

