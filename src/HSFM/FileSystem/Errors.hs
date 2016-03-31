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

{-# OPTIONS_HADDOCK ignore-exports #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |Provides error handling.
module HSFM.FileSystem.Errors where


import Control.Exception
import Control.Monad
  (
    when
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
import System.FilePath
  (
    equalFilePath
  )
import System.IO.Error
  (
    catchIOError
  )

import qualified System.Posix.Files as PF


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
  deriving (Show, Typeable)


instance Exception FmIOException




    ----------------------------
    --[ Path based functions ]--
    ----------------------------


throwFileDoesExist :: Path Abs -> IO ()
throwFileDoesExist fp =
  whenM (doesFileExist fp) (throw $ FileDoesExist $ P.fromAbs fp)


throwDirDoesExist :: Path Abs -> IO ()
throwDirDoesExist fp =
  whenM (doesDirectoryExist fp) (throw $ DirDoesExist $ P.fromAbs fp)


throwFileDoesNotExist :: Path Abs -> IO ()
throwFileDoesNotExist fp =
  whenM (doesFileExist fp) (throw $ FileDoesExist $ P.fromAbs fp)


throwDirDoesNotExist :: Path Abs -> IO ()
throwDirDoesNotExist fp =
  whenM (doesDirectoryExist fp) (throw $ DirDoesExist $ P.fromAbs fp)


throwSameFile :: Path Abs -- ^ will be canonicalized
              -> Path Abs -- ^ will be canonicalized
              -> IO ()
throwSameFile fp1 fp2 = do
  fp1' <- fmap P.fromAbs $ P.canonicalizePath fp1
  fp2' <- fmap P.fromAbs $ P.canonicalizePath fp2
  when (equalFilePath fp1' fp2') (throw $ SameFile fp1' fp2')


throwDestinationInSource :: Path Abs -- ^ will be canonicalized
                         -> Path Abs -- ^ will be canonicalized
                         -> IO ()
throwDestinationInSource source dest = do
  source'  <- P.canonicalizePath source
  cDestbase <- fmap P.dirname $ P.canonicalizePath dest
  let dest' = cDestbase P.</> P.basename dest
  when (source' `P.isParentOf` dest')
       (throw $ DestinationInSource (P.fromAbs dest') (P.fromAbs source'))


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


handleIOError :: (IOError -> IO a) -> IO a -> IO a
handleIOError a1 a2 = catchIOError a2 a1

