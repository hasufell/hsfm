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
import Data.ByteString
  (
    ByteString
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

import qualified System.Posix.Directory.ByteString as PFD
import System.Posix.Files.ByteString
  (
    fileAccess
  , getFileStatus
  )
import qualified System.Posix.Files.ByteString as PF


data FmIOException = FileDoesNotExist ByteString
                   | DirDoesNotExist ByteString
                   | PathNotAbsolute ByteString
                   | FileNotExecutable ByteString
                   | SameFile ByteString ByteString
                   | NotAFile ByteString
                   | NotADir ByteString
                   | DestinationInSource ByteString ByteString
                   | FileDoesExist ByteString
                   | DirDoesExist ByteString
                   | IsSymlink ByteString
                   | InvalidOperation String
                   | InvalidFileName
                   | Can'tOpenDirectory ByteString
                   | CopyFailed String
                   | MoveFailed String
  deriving (Typeable, Eq)


instance Show FmIOException where
  show (FileDoesNotExist fp) = "File does not exist:" ++ P.fpToString fp
  show (DirDoesNotExist fp) = "Directory does not exist: "
                              ++ P.fpToString fp
  show (PathNotAbsolute fp) = "Path not absolute: " ++ P.fpToString fp
  show (FileNotExecutable fp) = "File not executable: "
                                ++ P.fpToString fp
  show (SameFile fp1 fp2) = P.fpToString fp1
                            ++ " and " ++ P.fpToString fp2
                            ++ " are the same file!"
  show (NotAFile fp) = "Not a file: " ++ P.fpToString fp
  show (NotADir fp) = "Not a directory: " ++ P.fpToString fp
  show (DestinationInSource fp1 fp2) = P.fpToString fp1
                                       ++ " is contained in "
                                       ++ P.fpToString fp2
  show (FileDoesExist fp) = "File does exist: " ++ P.fpToString fp
  show (DirDoesExist fp) = "Directory does exist: " ++ P.fpToString fp
  show (IsSymlink fp) = "Is a symlink: " ++ P.fpToString fp
  show (InvalidOperation str) = "Invalid operation: " ++ str
  show InvalidFileName = "Invalid file name!"
  show (Can'tOpenDirectory fp) = "Can't open directory: "
                                 ++ P.fpToString fp
  show (CopyFailed str) = "Copying failed: " ++ str
  show (MoveFailed str) = "Moving failed: " ++ str



instance Exception FmIOException



isDestinationInSource :: FmIOException -> Bool
isDestinationInSource (DestinationInSource _ _) = True
isDestinationInSource _                         = False


isSameFile :: FmIOException -> Bool
isSameFile (SameFile _ _) = True
isSameFile _              = False


isFileDoesExist :: FmIOException -> Bool
isFileDoesExist (FileDoesExist _) = True
isFileDoesExist _                 = False


isDirDoesExist :: FmIOException -> Bool
isDirDoesExist (DirDoesExist _) = True
isDirDoesExist _                = False



    ----------------------------
    --[ Path based functions ]--
    ----------------------------


throwFileDoesExist :: Path Abs -> IO ()
throwFileDoesExist fp =
  whenM (doesFileExist fp) (throw . FileDoesExist
                                  . P.fromAbs $ fp)


throwDirDoesExist :: Path Abs -> IO ()
throwDirDoesExist fp =
  whenM (doesDirectoryExist fp) (throw . DirDoesExist
                                       . P.fromAbs $ fp)


throwFileDoesNotExist :: Path Abs -> IO ()
throwFileDoesNotExist fp =
  unlessM (doesFileExist fp) (throw . FileDoesNotExist
                                    . P.fromAbs $ fp)


throwDirDoesNotExist :: Path Abs -> IO ()
throwDirDoesNotExist fp =
  unlessM (doesDirectoryExist fp) (throw . DirDoesNotExist
                                         . P.fromAbs $ fp)


-- |Uses `isSameFile` and throws `SameFile` if it returns True.
throwSameFile :: Path Abs
              -> Path Abs
              -> IO ()
throwSameFile fp1 fp2 =
  whenM (sameFile fp1 fp2)
        (throw $ SameFile (P.fromAbs fp1) (P.fromAbs fp2))


-- |Check if the files are the same by examining device and file id.
-- This follows symbolic links.
sameFile :: Path Abs -> Path Abs -> IO Bool
sameFile fp1 fp2 =
  P.withAbsPath fp1 $ \fp1' -> P.withAbsPath fp2 $ \fp2' ->
    handleIOError (\_ -> return False) $ do
      fs1 <- getFileStatus fp1'
      fs2 <- getFileStatus fp2'

      if ((PF.deviceID fs1, PF.fileID fs1) ==
          (PF.deviceID fs2, PF.fileID fs2))
        then return True
        else return False


-- |Checks whether the destination directory is contained
-- within the source directory by comparing the device+file ID of the
-- source directory with all device+file IDs of the parent directories
-- of the destination.
throwDestinationInSource :: Path Abs -- ^ source dir
                         -> Path Abs -- ^ full destination, `dirname dest`
                                     --   must exist
                         -> IO ()
throwDestinationInSource source dest = do
  dest'   <- (\x -> maybe x (\y -> x P.</> y) $ P.basename dest)
             <$> (P.canonicalizePath $ P.dirname dest)
  dids <- forM (P.getAllParents dest') $ \p -> do
          fs <- PF.getSymbolicLinkStatus (P.fromAbs p)
          return (PF.deviceID fs, PF.fileID fs)
  sid <- fmap (\x -> (PF.deviceID x, PF.fileID x))
              $ PF.getFileStatus (P.fromAbs source)
  when (elem sid dids)
       (throw $ DestinationInSource (P.fromAbs dest)
                                    (P.fromAbs source))


-- |Checks if the given file exists and is not a directory.
-- Does not follow symlinks.
doesFileExist :: Path Abs -> IO Bool
doesFileExist fp =
  handleIOError (\_ -> return False) $ do
    fs  <- PF.getSymbolicLinkStatus (P.fromAbs fp)
    return $ not . PF.isDirectory $ fs


-- |Checks if the given file exists and is a directory.
-- Does not follow symlinks.
doesDirectoryExist :: Path Abs -> IO Bool
doesDirectoryExist fp =
  handleIOError (\_ -> return False) $ do
    fs  <- PF.getSymbolicLinkStatus (P.fromAbs fp)
    return $ PF.isDirectory fs


-- |Checks whether a file or folder is writable.
isWritable :: Path Abs -> IO Bool
isWritable fp =
  handleIOError (\_ -> return False) $
    fileAccess (P.fromAbs fp) False True False


-- |Checks whether the directory at the given path exists and can be
-- opened. This invokes `openDirStream` which follows symlinks.
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
          (throw . Can'tOpenDirectory . P.fromAbs $ fp)



    --------------------------------
    --[ Error handling functions ]--
    --------------------------------


-- |Carries out an action, then checks if there is an IOException and
-- a specific errno. If so, then it carries out another action, otherwise
-- it rethrows the error.
catchErrno :: [Errno] -- ^ errno to catch
           -> IO a    -- ^ action to try, which can raise an IOException
           -> IO a    -- ^ action to carry out in case of an IOException and
                      --   if errno matches
           -> IO a
catchErrno en a1 a2 =
  catchIOError a1 $ \e -> do
    errno <- getErrno
    if errno `elem` en
      then a2
      else ioError e


-- |Execute the given action and retrow IO exceptions as a new Exception
-- that have the given errno. If errno does not match the exception is rethrown
-- as is.
rethrowErrnoAs :: Exception e
               => [Errno]       -- ^ errno to catch
               -> e             -- ^ rethrow as if errno matches
               -> IO a          -- ^ action to try
               -> IO a
rethrowErrnoAs en fmex action = catchErrno en action (throw fmex)



-- |Like `catchIOError`, with arguments swapped.
handleIOError :: (IOError -> IO a) -> IO a -> IO a
handleIOError = flip catchIOError


-- |Like `bracket`, but allows to have different clean-up
-- actions depending on whether the in-between computation
-- has raised an exception or not. 
bracketeer :: IO a        -- ^ computation to run first
           -> (a -> IO b) -- ^ computation to run last, when
                          --   no exception was raised
           -> (a -> IO b) -- ^ computation to run last,
                          --   when an exception was raised
           -> (a -> IO c) -- ^ computation to run in-between
           -> IO c
bracketeer before after afterEx thing =
  mask $ \restore -> do
    a <- before
    r <- restore (thing a) `onException` afterEx a
    _ <- after a
    return r
