{--
HSFM, a filemanager written in Haskell.
Copyright (C) 2016 Julian Ospald

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
version 2 as published by the Free Software Foundation.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
--}

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

-- |Provides error handling.
module HSFM.FileSystem.Errors where


import Control.Exception
import Control.Monad
  (
    when
  , forM
  )
import Data.Typeable
import Foreign.C.Error
  (
    getErrno
  , Errno
  )
import qualified HPath as P
import HPath
    (
      Abs
    , Path
    )
import HSFM.Utils.IO
import System.IO.Error
  (
    catchIOError
  )

import qualified System.Posix.Files.ByteString as PF
import qualified System.Posix.Directory.ByteString as PFD


data FmIOException = FileDoesNotExist String
                   | DirDoesNotExist String
                   | PathNotAbsolute String
                   | FileNotExecutable String
                   | SameFile String String
                   | NotAFile String
                   | NotADir String
                   | DestinationInSource String String
                   | FileDoesExist String
                   | DirDoesExist String
                   | IsSymlink String
                   | InvalidOperation String
                   | InvalidFileName
                   | Can'tOpenDirectory String
  deriving (Show, Typeable)


instance Exception FmIOException




    ----------------------------
    --[ Path based functions ]--
    ----------------------------


throwFileDoesExist :: Path Abs -> IO ()
throwFileDoesExist fp =
  whenM (doesFileExist fp) (throw . FileDoesExist
                                  . P.fpToString . P.fromAbs $ fp)


throwDirDoesExist :: Path Abs -> IO ()
throwDirDoesExist fp =
  whenM (doesDirectoryExist fp) (throw . DirDoesExist
                                       . P.fpToString . P.fromAbs $ fp)


throwFileDoesNotExist :: Path Abs -> IO ()
throwFileDoesNotExist fp =
  whenM (doesFileExist fp) (throw . FileDoesExist
                                  . P.fpToString . P.fromAbs $ fp)


throwDirDoesNotExist :: Path Abs -> IO ()
throwDirDoesNotExist fp =
  whenM (doesDirectoryExist fp) (throw . DirDoesExist
                                       . P.fpToString . P.fromAbs $ fp)


throwSameFile :: Path Abs -- ^ will be canonicalized
              -> Path Abs -- ^ will be canonicalized
              -> IO ()
throwSameFile fp1 fp2 = do
  fp1' <- fmap P.fromAbs $ P.canonicalizePath fp1
  -- TODO: clean this up... if canonicalizing fp2 fails we try to
  --       canonicalize `dirname fp2`
  fp2' <- catchIOError (fmap P.fromAbs $ P.canonicalizePath fp2)
                       (\_ -> fmap P.fromAbs
                              $ (P.</> P.basename fp2)
                                <$> (P.canonicalizePath $ P.dirname fp2))
  when (P.equalFilePath fp1' fp2') (throw $ SameFile (P.fpToString fp1')
                                                     (P.fpToString fp2'))


-- |Checks whether the destination directory is contained
-- within the source directory by comparing the device+file ID of the
-- source directory with all device+file IDs of the parent directories
-- of the destination.
throwDestinationInSource :: Path Abs -- ^ source dir
                         -> Path Abs -- ^ full destination, `dirname dest`
                                     --   must exist
                         -> IO ()
throwDestinationInSource source dest = do
  source' <- P.canonicalizePath source
  dest'   <- (P.</> P.basename dest) <$> (P.canonicalizePath $ P.dirname dest)
  dids <- forM (P.getAllParents dest') $ \p -> do
          fs <- PF.getSymbolicLinkStatus (P.fromAbs p)
          return (PF.deviceID fs, PF.fileID fs)
  sid <- fmap (\x -> (PF.deviceID x, PF.fileID x))
              $ PF.getSymbolicLinkStatus (P.fromAbs source')
  when (elem sid dids)
       (throw $ DestinationInSource (P.fpToString $ P.fromAbs dest)
                                    (P.fpToString $ P.fromAbs source))


-- |Checks if the given file exists and is not a directory. This follows
-- symlinks, but will return True if the symlink is broken.
doesFileExist :: Path Abs -> IO Bool
doesFileExist fp =
  handleIOError (\_ -> return False) $ do
    fp' <- fmap P.fromAbs $ P.canonicalizePath fp
    fs  <- PF.getFileStatus fp'
    return $ not . PF.isDirectory $ fs


-- |Checks if the given file exists and is a directory. This follows
-- symlinks, but will return False if the symlink is broken.
doesDirectoryExist :: Path Abs -> IO Bool
doesDirectoryExist fp =
  handleIOError (\_ -> return False) $ do
    fp' <- fmap P.fromAbs $ P.canonicalizePath fp
    fs  <- PF.getFileStatus fp'
    return $ PF.isDirectory fs


-- |Checks whether the directory at the given path exists and can be
-- opened. This invokes `openDirStream`.
canOpenDirectory :: Path Abs -> IO Bool
canOpenDirectory fp =
  handleIOError (\_ -> return False) $ do
    bracket (PFD.openDirStream . P.fromAbs $ fp)
            PFD.closeDirStream
            (\_ -> return ())
    return True


-- |Throws a `Can'tOpenDirectory` FmIOException if the directory at the given
-- path cannot be opened.
throwCantOpenDirectory :: Path Abs -> IO ()
throwCantOpenDirectory fp =
  unlessM (canOpenDirectory fp)
          (throw . Can'tOpenDirectory . show . P.fromAbs $ fp)



    --------------------------------
    --[ Error handling functions ]--
    --------------------------------


-- |Carries out an action, then checks if there is an IOException and
-- a specific errno. If so, then it carries out another action, otherwise
-- it rethrows the error.
catchErrno :: Errno   -- ^ errno to catch
           -> IO a    -- ^ action to try, which can raise an IOException
           -> IO a    -- ^ action to carry out in case of an IOException and
                      --   if errno matches
           -> IO a
catchErrno en a1 a2 =
  catchIOError a1 $ \e -> do
    errno <- getErrno
    if errno == en
      then a2
      else ioError e


-- |Execute the given action and retrow IO exceptions as a new Exception
-- that have the given errno. If errno does not match the exception is rethrown
-- as is.
rethrowErrnoAs :: Exception e
               => Errno         -- ^ errno to catch
               -> e             -- ^ rethrow as if errno matches
               -> IO a          -- ^ action to try
               -> IO a
rethrowErrnoAs en fmex action = catchErrno en action (throw fmex)



-- |Like `catchIOError`, with arguments swapped.
handleIOError :: (IOError -> IO a) -> IO a -> IO a
handleIOError a1 a2 = catchIOError a2 a1

