{-# LANGUAGE CPP #-}

module Data.Yteratee.IO.Base (
#if defined(USE_WINDOWS)
  module Data.Yteratee.IO.Windows,
#endif
#if defined(USE_POSIX)
  module Data.Yteratee.IO.Posix,
#else
  FileOffset
#endif
)
where

#if defined(USE_WINDOWS)
import Data.Yteratee.IO.Windows
#endif

-- Provide the FileOffset type, which is available in Posix modules
-- and maybe Windows
#if defined(USE_POSIX)
import Data.Yteratee.IO.Posix
#else
type FileOffset = Integer
#endif
