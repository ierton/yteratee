{-# LANGUAGE CPP #-}

-- |Random and Binary IO with generic Iteratees.

module Data.Yteratee.IO(
  defaultBufSize,
  H.enumHandle,
  enumFile,
#if defined(USE_POSIX)
  FD.enumFd,
#endif
  fileDriver,
  fileDriverVBuf
)

where

import Data.Yteratee.Base
import Data.Yteratee.IO.ReadableChunk
import qualified Data.Yteratee.IO.Handle as H

#if defined(USE_POSIX)
import qualified Data.Yteratee.IO.Fd as FD
#endif

import Control.Monad.CatchIO

-- | The default buffer size.
defaultBufSize :: Int
defaultBufSize = 1024

-- If Posix is available, use the fileDriverRandomFd as fileDriverRandom.  Otherwise, use a handle-based variant.
#if defined(USE_POSIX)

enumFile
  :: (MonadCatchIO m, ReadableChunk s el) =>
     Int
     -> FilePath
     -> YEnumerator s m a
enumFile = FD.enumFile

-- |Process a file using the given Iteratee.  This function wraps
-- enumFd as a convenience.
fileDriver
  :: (MonadCatchIO m, ReadableChunk s el) =>
     Yteratee s m a
     -> FilePath
     -> m a
fileDriver = FD.fileDriverFd defaultBufSize

-- |A version of fileDriver with a user-specified buffer size (in elements).
fileDriverVBuf
  :: (MonadCatchIO m, ReadableChunk s el) =>
     Int
     -> Yteratee s m a
     -> FilePath
     -> m a
fileDriverVBuf = FD.fileDriverFd

#else
-- -----------------------------------------------
-- Handle-based operations for compatibility.

-- |Process a file using the given Yteratee.  This function wraps
-- @enumHandle@ as a convenience.
fileDriver ::
 (MonadCatchIO m, ReadableChunk s el) =>
  Yteratee s m a
  -> FilePath
  -> m a
fileDriver = H.fileDriverHandle defaultBufSize

-- |A version of fileDriver with a user-specified buffer size (in elements).
fileDriverVBuf ::
 (MonadCatchIO m, ReadableChunk s el) =>
  Int
  -> Yteratee s m a
  -> FilePath
  -> m a
fileDriverVBuf = H.fileDriverHandle

enumFile
  :: (MonadCatchIO m, ReadableChunk s el) =>
     Int
     -> FilePath
     -> YEnumerator s m a
enumFile = H.enumFile

#endif
