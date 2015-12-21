{-# OPTIONS_HADDOCK ignore-exports #-}

-- |This module provides all the atomic IO related file operations like
-- copy, delete, move and so on. It operates only on FilePaths and reads
-- all necessary file information manually in order to stay atomic and not
-- rely on the state of passed objects.
--
-- It would be nicer to pass states around, but the filesystem state changes
-- too quickly and cannot be relied upon. Lazy implementations of filesystem
-- trees have been tried as well, but they can introduce subtle bugs.
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
  , fileAccess
  , getFileStatus
  )
import System.Process
  (
    spawnProcess
  , ProcessHandle
  )

import qualified System.Directory as SD

import qualified System.Posix.Files as PF


-- TODO: modify the DTZipper directly after file operations!?
-- TODO: file operations should be threaded and not block the UI
-- TODO: canonicalize paths?


-- |Data type describing an actual file operation that can be
-- carried out via `doFile`. Useful to build up a list of operations
-- or delay operations.
data FileOperation = FCopy    Copy
                   | FMove    Move
                   | FDelete  FilePath
                   | FOpen    FilePath
                   | FExecute FilePath [String]
                   | None


-- |Data type describing partial or complete file copy operation.
-- CC stands for a complete operation and can be used for `runFileOp`.
data Copy = CP1 FilePath
          | CP2 FilePath FilePath
          | CC  FilePath FilePath DirCopyMode


-- |Data type describing partial or complete file move operation.
-- MC stands for a complete operation and can be used for `runFileOp`.
data Move = MP1 FilePath
          | MC  FilePath FilePath


-- |Directory copy modes.
data DirCopyMode = Strict  -- ^ fail if the target directory already exists
                 | Merge   -- ^ overwrite files if necessary
                 | Replace -- ^ remove target directory before copying


-- |Run a given FileOperation. If the FileOperation is partial, it will
-- be returned.
--
-- The operation may fail with:
--
-- * anything that `copyFileToDir`, `easyDelete`, `openFile`,
-- `executeFile` throws
runFileOp :: FileOperation -> IO (Maybe FileOperation)
runFileOp (FCopy (CC from to cm)) = easyCopy cm from to >> return Nothing
runFileOp (FCopy fo) = return $ Just $ FCopy fo
runFileOp (FDelete fp)       = easyDelete fp >> return Nothing
runFileOp (FOpen fp)         = openFile fp >> return Nothing
runFileOp (FExecute fp args) = executeFile fp args >> return Nothing
runFileOp _                  = return Nothing



    --------------------
    --[ File Copying ]--
    --------------------


-- TODO: allow renaming
-- |Copies a directory to the given destination with the specified
-- `DirCopyMode`.
--
-- The operation may fail with:
--
-- * `DirDoesNotExist` if the source or destination directory does not exist
-- * `DestinationInSource` if the destination directory is contained within
-- the source directory
-- * `SameFile` if the source and destination directory are the same
-- * `DirDoesExist` if the target directory already exists during the Strict
-- copy mode
-- * anything that `copyFileToDir`, `getFileStatus`, `createDirectory`,
-- `easyDelete`, `readSymbolicLink`, `createDirectoryIfMissing`,
-- `removeDirectoryRecursive`, `createSymbolicLink`, `copyDir`,
-- `copyFileToDir`, `getDirectoryContents` throws
copyDir :: DirCopyMode
        -> FilePath  -- ^ source dir
        -> FilePath  -- ^ destination dir
        -> IO ()
copyDir cm from' to' = do
  from <- canonicalizePath' from'
  to   <- canonicalizePath' to'
  go from to
  where
    go from to = do
      let fn = takeFileName from
          destdir = to </> fn

      dirSanityThrow from
      dirSanityThrow to
      throwDestinationInSource from to
      throwSameFile from destdir

      createDestdir destdir

      contents <- getDirsFiles from

      for_ contents $ \f -> do
          let ffn = from </> f
          fs <- PF.getSymbolicLinkStatus ffn
          case (PF.isSymbolicLink fs, PF.isDirectory fs) of
            (True, _) -> recreateSymlink destdir f ffn
            (_, True) -> copyDir cm ffn destdir
            (_, _)    -> copyFileToDir ffn destdir
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
    recreateSymlink destdir n f = do
      let sympoint = destdir </> n

      case cm of
        -- delete old file/dir to be able to create symlink
        Merge -> easyDelete sympoint
        _     -> return ()

      symname <- readSymbolicLink f
      createSymbolicLink symname sympoint


-- |Copies the given file.
--
-- The operation may fail with:
--
-- * `PathNotAbsolute` either the source or destination file is not an
-- absolute path
-- * `FileDoesNotExist` the source file does not exist
-- * `DirDoesNotExist` the target directory does not exist
-- * `PathNotAbsolute` if either of the filepaths are not absolute
-- * `SameFile` if the source and destination files are the same
-- * anything that `canonicalizePath` or `System.Directory.copyFile` throws
copyFile :: FilePath  -- ^ source file
         -> FilePath  -- ^ destination file
         -> IO ()
copyFile from' to' = do
  from <- canonicalizePath' from'
  tod  <- canonicalizePath' (baseDir to')
  let to = tod </> takeFileName to'
  fileSanityThrow from
  throwNotAbsolute to
  throwDirDoesExist to
  toC <- canonicalizePath' (takeDirectory to)
  let to' = toC </> takeFileName to
  throwSameFile from to'
  SD.copyFile from to'



-- |Copies the given file to the given dir with the same filename.
--
-- The operation may fail with:
--
-- * `DirDoesNotExist` if the target directory does not exist
-- * `PathNotAbsolute` if the target directory is not absolute
-- * anything that `copyFile` throws
copyFileToDir :: FilePath -> FilePath -> IO ()
copyFileToDir from' to' = do
  from <- canonicalizePath' from'
  to   <- canonicalizePath' to'
  let name = takeFileName from
  dirSanityThrow to
  copyFile from (to </> name)


easyCopy :: DirCopyMode -> FilePath -> FilePath -> IO ()
easyCopy cm from to = doFileOrDir from (copyDir cm from to)
                                       (copyFileToDir from to)



    ---------------------
    --[ File Deletion ]--
    ---------------------


-- TODO: misbehaves on symlinks
-- |Deletes the given file or symlink.
--
-- The operation may fail with:
--
-- * `FileDoesNotExist` if the file does not exist
-- * `PathNotAbsolute` if the file is not absolute
-- * anything that `removeFile` throws
deleteFile :: FilePath -> IO ()
deleteFile fp' = do
  fp <- canonicalizePath' fp'
  fileSanityThrow fp
  removeFile fp


-- TODO: misbehaves on symlinks
-- |Deletes the given directory.
--
-- The operation may fail with:
--
-- * `DirDoesNotExist` if the dir does not exist
-- * `PathNotAbsolute` if the dir is not absolute
-- * anything that `removeDirectory` throws
deleteDir :: FilePath -> IO ()
deleteDir fp' = do
  fp <- canonicalizePath' fp'
  dirSanityThrow fp
  removeDirectory fp


-- |Deletes the given directory recursively.
--
-- The operation may fail with:
--
-- * `DirDoesNotExist` if the dir does not exist
-- * `PathNotAbsolute` if the dir is not absolute
-- * anything that `removeDirectoryRecursive` throws
deleteDirRecursive :: FilePath -> IO ()
deleteDirRecursive fp' = do
  fp <- canonicalizePath' fp'
  dirSanityThrow fp
  removeDirectoryRecursive fp


-- |Deletes a file or directory, whatever it may be.
--
-- The operation may fail with:
--
-- * `DirDoesNotExist`/`FileDoesNotExist` if the file/dir does not exist
-- * `PathNotAbsolute` if the file/dir is not absolute
-- * anything that `deleteDir`/`deleteFile` throws
easyDelete :: FilePath -> IO ()
easyDelete fp' = do
  fp <- canonicalizePath' fp'
  doFileOrDir fp (deleteDir fp) (deleteFile fp)



    --------------------
    --[ File Opening ]--
    --------------------


-- |Opens a file appropriately by invoking xdg-open.
--
-- The operation may fail with:
--
-- * `FileDoesNotExist` if the file does not exist
-- * `PathNotAbsolute` if the file is not absolute
openFile :: FilePath
         -> IO ProcessHandle
openFile fp' = do
  fp <- canonicalizePath' fp'
  fileSanityThrow fp
  spawnProcess "xdg-open" [fp]


-- |Executes a program with the given arguments.
--
-- The operation may fail with:
--
-- * `FileDoesNotExist` if the program does not exist
-- * `PathNotAbsolute` if the program is not absolute
-- * `FileNotExecutable` if the program is not executable
executeFile :: FilePath      -- ^ program
            -> [String]      -- ^ arguments
            -> IO ProcessHandle
executeFile prog' args = do
  prog <- canonicalizePath' prog'
  fileSanityThrow prog
  unlessM (fileAccess prog False False True) (throw $ FileNotExecutable prog)
  spawnProcess prog args




    --------------------
    --[ Utilities ]--
    --------------------


-- |Executes either a directory or file related IO action, depending on
-- the input filepath.
--
-- The operation may fail with:
--
-- * `throwFileDoesNotExist` if the filepath is neither a file or directory
doFileOrDir :: FilePath -> IO () -> IO () -> IO ()
doFileOrDir fp' iod iof = do
  fp <- canonicalizePath' fp'
  isD <- doesDirectoryExist fp
  isF <- doesFileExist fp
  case (isD, isF) of
    (True, False) -> do
      dirSanityThrow fp
      iod
    (False, True) -> do
      fileSanityThrow fp
      iof
    _             -> throwFileDoesNotExist fp
