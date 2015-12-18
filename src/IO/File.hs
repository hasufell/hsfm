{-# OPTIONS_HADDOCK ignore-exports #-}

module IO.File where


import Control.Exception
  (
    throw
  )
import Control.Monad
  (
    unless
  , void
  )
import Data.DirTree
import Data.DirTree.Zipper
import Data.Foldable
  (
    for_
  )
import IO.Error
import IO.Utils
import System.Directory
  (
    canonicalizePath
  , createDirectory
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , executable
  , removeDirectory
  , removeDirectoryRecursive
  , removeFile
  )
import System.FilePath
  (
    equalFilePath
  , isAbsolute
  , takeFileName
  , takeDirectory
  , (</>)
  )
import System.Posix.Files
  (
    createSymbolicLink
  , readSymbolicLink
  )
import System.Process
  (
    spawnProcess
  , ProcessHandle
  )

import qualified System.Directory as SD


-- TODO: modify the DTZipper directly after file operations!?
-- TODO: file operations should be threaded and not block the UI


-- |Data type describing an actual file operation that can be
-- carried out via `doFile`. Useful to build up a list of operations
-- or delay operations.
data FileOperation = FCopy    Copy
                   | FMove    Move
                   | FDelete  DTInfoZipper
                   | FOpen    DTInfoZipper
                   | FExecute DTInfoZipper [String]
                   | None


data Copy = CP1 DTInfoZipper
          | CP2 DTInfoZipper DTInfoZipper
          | CC  DTInfoZipper DTInfoZipper DirCopyMode


data Move = MP1 DTInfoZipper
          | MC  DTInfoZipper DTInfoZipper


-- |Directory copy modes.
-- Strict means we fail if the target directory already exists.
-- Merge means we keep the old directories/files, but overwrite old files
-- on collision.
-- Replace means the target directory will be removed recursively before
-- performing the copy operation.
data DirCopyMode = Strict
                 | Merge
                 | Replace


runFileOp :: FileOperation -> IO (Maybe FileOperation)
runFileOp (FCopy (CC from@(File {}, _) to cm)) =
  copyFileToDir from to >> return Nothing
runFileOp (FCopy (CC from@(Dir {}, _) to cm)) =
  copyDir cm from to >> return Nothing
runFileOp fo@(FCopy _) = return $ Just fo
runFileOp (FDelete fp)       = easyDelete fp >> return Nothing
runFileOp (FOpen fp)         = openFile fp >> return Nothing
runFileOp (FExecute fp args) = executeFile fp args >> return Nothing
runFileOp _                  = return Nothing



-- TODO: allow renaming
-- |Copies a directory to the given destination.
copyDir :: DirCopyMode
        -> DTInfoZipper  -- ^ source dir
        -> DTInfoZipper  -- ^ destination dir
        -> IO ()
copyDir cm from@(Dir fn _ _, _) to@(Dir {}, _) = do
  let fromp = getFullPath from
      top   = getFullPath to
      destdir = getFullPath to </> fn

  dirSanityThrow fromp
  dirSanityThrow top
  throwDestinationInSource fromp top

  createDestdir destdir

  newDest <- zipLazy mkDirInfo mkFileInfo destdir

  for_ (goAllDown from) $ \f ->
    -- TODO: maybe do this strict?
    case f of
      -- recreate symlink
      sz@(Dir { name = n, dir = (DirInfo { sym = True }) }, _)  ->
        recreateSymlink newDest n sz
      sz@(Dir {}, _)  ->
        copyDir cm sz newDest
      sz@(File {}, _) ->
        copyFileToDir sz newDest
  where
    createDestdir destdir =
      case cm of
        Merge   ->
          createDirectoryIfMissing False destdir
        Strict  -> do
          throwDirDoesExist destdir
          createDirectory destdir
        Replace -> do
          whenM (doesDirectoryExist destdir) (removeDirectoryRecursive destdir)
          createDirectory destdir
    recreateSymlink newDest n sz = do
      let sympoint = getFullPath newDest </> n

      case cm of
        Merge ->
          -- delete old file/dir to be able to create symlink
          for_ (goDown n newDest) $ \odtz ->
            easyDelete odtz
        _     -> return ()

      symname <- readSymbolicLink (getFullPath sz)
      createSymbolicLink symname sympoint

copyDir _ from@(File _ _, _) _ = throw $ NotADir (getFullPath from)
copyDir _ _ to@(File _ _, _)   = throw $ NotADir (getFullPath to)


-- |Copies the given file.
--
-- This will throw an exception if any of the filepaths are not absolute
-- and an exception if the source file does not exist.
--
-- If the destination file already exists, it will be replaced.
copyFile :: DTZipper a b  -- ^ source file
         -> FilePath      -- ^ destination file
         -> IO ()
copyFile from@(File name _, _) to = do
  let fp = getFullPath from
  fileSanityThrow fp
  throwNotAbsolute to
  throwDirDoesExist to
  toC <- canonicalizePath (takeDirectory to)
  let to' = toC </> takeFileName to
  throwSameFile fp to'
  SD.copyFile fp to'
copyFile from _ = throw $ NotAFile (getFullPath from)


-- |Copies the given file to the given dir with the same filename.
--
-- This is just a convenience wrapper around `copyFile`.
copyFileToDir :: DTZipper a b  -- ^ source file
              -> DTZipper a b  -- ^ destination
              -> IO ()
copyFileToDir from@(File name _, _) to@(Dir {}, _) = do
  let dp = getFullPath to
  dirSanityThrow dp
  copyFile from (dp </> name)
copyFileToDir from (Dir {}, _) = throw $ NotAFile (getFullPath from)
copyFileToDir _    to          = throw $ NotADir (getFullPath to)


-- |Copies the given file, regardless of whether the destination is
-- a file or a directory. This is a wrapper around `copyFile` and
-- `copyFileToDir`.
easyCopyFile :: DTZipper a b -> Either FilePath (DTZipper a b) -> IO ()
easyCopyFile from (Left to)  = copyFile from to
easyCopyFile from (Right to) = copyFileToDir from to


-- |Deletes the given file or symlink.
--
-- This will throw an exception if the filepath is not absolute
-- or the file does not exist.
--
-- It also throws exceptions from `removeFile`.
deleteFile :: DTInfoZipper -> IO ()
deleteFile dtz@(File {}, _) = do
  let fp = getFullPath dtz
  fileSanityThrow fp
  removeFile fp
deleteFile dtz@(Dir { dir = (DirInfo { sym = True }) }, _) = do
  let fp = getFullPath dtz
  throwNotAbsolute fp
  removeFile fp
deleteFile dtz = throw $ NotAFile (getFullPath dtz)


-- |Deletes the given directory. Does not work on symlinks.
--
-- This will throw an exception if the filepath is not absolute
-- or the directory does not exist.
--
-- It also throws exceptions from `removeDirectory`.
deleteDir :: DTInfoZipper -> IO ()
deleteDir dtz@(Dir {}, _) = do
  let fp = getFullPath dtz
  dirSanityThrow fp
  removeDirectory fp
deleteDir dtz = throw $ NotADir (getFullPath dtz)


-- |Deletes the given directory recursively. Does not work on symlinks.
--
-- This will throw an exception if the filepath is not absolute
-- or the directory does not exist.
--
-- It also throws exceptions from `removeDirectoryRecursive`.
deleteDirRecursive :: DTInfoZipper -> IO ()
deleteDirRecursive dtz@(Dir {}, _) = do
  let fp = getFullPath dtz
  dirSanityThrow fp
  removeDirectoryRecursive fp
deleteDirRecursive dtz = throw $ NotADir (getFullPath dtz)


-- |Deletes a file or directory, whatever it may be.
easyDelete :: DTInfoZipper -> IO ()
easyDelete dtz@(File {}, _) = deleteFile dtz
easyDelete dtz@(Dir { dir = (DirInfo { sym = True }) }, _) = deleteFile dtz
easyDelete dtz@(Dir {}, _)  = deleteDir dtz


-- |Opens a file appropriately by invoking xdg-open.
--
-- This will throw an exception if the filepath is not absolute
-- or the file does not exist.
openFile :: DTZipper a b
         -> IO ProcessHandle
openFile dtz@(File {}, _) = do
  let fp = getFullPath dtz
  fileSanityThrow fp
  spawnProcess "xdg-open" [fp]
openFile dtz = throw $ NotAFile (getFullPath dtz)


-- |Executes a program with the given arguments.
--
-- This will throw an exception if the filepath is not absolute
-- or the file does not exist. It will also throw an exception
-- if the file is not executable.
executeFile :: DTInfoZipper      -- ^ program
            -> [String]          -- ^ arguments
            -> IO ProcessHandle
executeFile dtz@(File { file = (FileInfo { permissions = p }) }, _) args = do
  let fp = getFullPath dtz
  fileSanityThrow fp
  unless (executable p) (throw $ FileNotExecutable fp)
  spawnProcess fp args
executeFile dtz  _ = throw $ NotAFile (getFullPath dtz)

