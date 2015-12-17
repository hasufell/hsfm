{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE DeriveDataTypeable #-}

module IO.Error where


import Control.Exception
import Control.Monad
  (
    mzero
  , MonadPlus
  )
import Data.Typeable


data FmIOException = FileDoesNotExist String
                   | PathNotAbsolute String
                   | FileNotExecutable String
  deriving (Show, Typeable)


instance Exception FmIOException

