{-# LANGUAGE CPP, ScopedTypeVariables #-}

-- |Random and Binary IO with generic Iteratees, using File Descriptors for IO.
-- when available, these are the preferred functions for performing IO as they
-- run in constant space and function properly with sockets, pipes, etc.

module Data.Yteratee.IO.Fd(
#if defined(USE_POSIX)
  -- * File enumerators
  -- ** FileDescriptor based enumerators for monadic iteratees
  enumFd
  ,enumFile
  -- * Iteratee drivers
  ,fileDriverFd
#endif
)

where

#if defined(USE_POSIX)
import Data.Yteratee.Base
import Data.Yteratee.IO.Base
import Data.Yteratee.IO.ReadableChunk

import Control.Concurrent (yield)
import Control.Exception
import Control.Monad
import Control.Monad.CatchIO as CIO
import Control.Monad.IO.Class

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

--import System.IO (SeekMode(..))

import System.Posix hiding (FileOffset)

-- ------------------------------------------------------------------------
-- Binary Random IO enumerators

makefdCallback ::
  (MonadIO m, ReadableChunk s el) =>
  Ptr el
  -> ByteCount
  -> Fd
  -> (st -> m (Either (Maybe SomeException) (st, s)))
makefdCallback p bufsize fd st = do
  n <- liftIO $ myfdRead fd (castPtr p) bufsize
  case n of
    Left  _  -> return $ Left $ Just (error "myfdRead failed")
    Right 0  -> liftIO yield >> return (Left Nothing)
    Right n' -> liftM (\s -> Right (st, s)) $ readFromPtr p (fromIntegral n')

-- |The enumerator of a POSIX File Descriptor.  This version enumerates
-- over the entire contents of a file, in order, unless stopped by
-- the iteratee.  In particular, seeking is not supported.
enumFd
  :: forall s el m a.(ReadableChunk s el, MonadCatchIO m) =>
     Int
     -> Fd
     -> YEnumerator s m a
enumFd bs fd iter =
  let bufsize = bs * (sizeOf (undefined :: el))
  in CIO.bracket (liftIO $ mallocBytes bufsize)
                 (liftIO . free)
                 (\p -> enumFromCallback (makefdCallback p (fromIntegral bufsize) fd) () iter)

fileDriver
  :: (MonadCatchIO m, ReadableChunk s el) =>
     (Int -> Fd -> YEnumerator s m a)
     -> Int
     -> Yteratee s m a
     -> FilePath
     -> m a
fileDriver enumf bufsize iter filepath = CIO.bracket
  (liftIO $ openFd filepath ReadOnly Nothing defaultFileFlags)
  (liftIO . closeFd)
  (run <=< flip (enumf bufsize) iter)

-- |Process a file using the given @Iteratee@.
fileDriverFd
  :: (MonadCatchIO m, ReadableChunk s el) =>
     Int -- ^Buffer size (number of elements)
     -> Yteratee s m a
     -> FilePath
     -> m a
fileDriverFd = fileDriver enumFd

enumFile' :: (MonadCatchIO m, ReadableChunk s el) =>
  (Int -> Fd -> YEnumerator s m a)
  -> Int -- ^Buffer size
  -> FilePath
  -> YEnumerator s m a
enumFile' enumf bufsize filepath iter = CIO.bracket
  (liftIO $ openFd filepath ReadOnly Nothing defaultFileFlags)
  (liftIO . closeFd)
  (flip (enumf bufsize) iter)

enumFile ::
  (MonadCatchIO m, ReadableChunk s el)
  => Int                 -- ^Buffer size
  -> FilePath
  -> YEnumerator s m a
enumFile = enumFile' enumFd

#endif
