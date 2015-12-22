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


import Control.Applicative
  (
    (<$>)
  )
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


-- TODO: file operations should be threaded and not block the UI


-- |Data type describing an actual file operation that can be
-- carried out via `doFile`. Useful to build up a list of operations
-- or delay operations.
data FileOperation = FCopy    Copy
                   | FMove    Move
                   | FDelete  (AnchoredFile FileInfo FileInfo)
                   | FOpen    (AnchoredFile FileInfo FileInfo)
                   | FExecute (AnchoredFile FileInfo FileInfo) [String]
                   | None


-- |Data type describing partial or complete file copy operation.
-- CC stands for a complete operation and can be used for `runFileOp`.
data Copy = CP1 (AnchoredFile FileInfo FileInfo)
          | CP2 (AnchoredFile FileInfo FileInfo)
                (AnchoredFile FileInfo FileInfo)
          | CC  (AnchoredFile FileInfo FileInfo)
                (AnchoredFile FileInfo FileInfo)
                DirCopyMode


-- |Data type describing partial or complete file move operation.
-- MC stands for a complete operation and can be used for `runFileOp`.
data Move = MP1 (AnchoredFile FileInfo FileInfo)
          | MC  (AnchoredFile FileInfo FileInfo)
                (AnchoredFile FileInfo FileInfo)


-- |Directory copy modes.
data DirCopyMode = Strict  -- ^ fail if the target directory already exists
                 | Merge   -- ^ overwrite files if necessary
                 | Replace -- ^ remove target directory before copying


-- |Run a given FileOperation. If the FileOperation is partial, it will
-- be returned.
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
copyDir :: DirCopyMode
        -> AnchoredFile FileInfo FileInfo  -- ^ source dir
        -> AnchoredFile FileInfo FileInfo  -- ^ destination dir
        -> IO ()
copyDir cm (IsSymL True) _
  = return ()
copyDir cm from@(_ :/ Dir fromn _)
             to@(_ :/ Dir {})
  = do
    let fromp    = fullPath from
        top      = fullPath to
        destdirp = fullPath to </> fromn
    print destdirp
    throwDestinationInSource fromp destdirp
    throwSameFile fromp destdirp

    createDestdir destdirp
    destdir <- Data.DirTree.readFile destdirp

    contents <- readDirectory' (fullPath from)

    for_ contents $ \f ->
      case f of
        (IsSymL True) -> recreateSymlink f destdir
        (_ :/ Dir {}) -> copyDir cm f destdir
        (_ :/ RegFile {}) -> copyFileToDir f destdir
        _                 -> return ()
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
    recreateSymlink' f destdir = do
      let destfilep = fullPath destdir </> (name . file $ f)
      destfile <- Data.DirTree.readFile destfilep

      _ <- case cm of
        -- delete old file/dir to be able to create symlink
        Merge -> easyDelete destfile
        _     -> return ()

      recreateSymlink f destdir
copyDir _ _ _ = return ()


-- |Recreate a symlink.
recreateSymlink :: AnchoredFile FileInfo FileInfo  -- ^ the old symlink file
                -> AnchoredFile FileInfo FileInfo  -- ^ destination dir of the
                                                   --   new symlink file
                -> IO ()
recreateSymlink symf@(IsSymL True)
                symdest@(_ :/ Dir {})
  = do
    symname <- readSymbolicLink (fullPath symf)
    createSymbolicLink symname (fullPath symdest </> (name . file $ symf))
recreateSymlink _ _ = return ()


-- |Copies the given file to the given file destination. Not symlinks.
copyFile :: AnchoredFile FileInfo FileInfo  -- ^ source file
         -> AnchoredFile FileInfo FileInfo  -- ^ destination file
         -> IO ()
copyFile (IsSymL True) _ = return ()
copyFile from@(_ :/ RegFile {}) to@(_ :/ RegFile {}) = do
  let from' = fullPath from
      to'   = fullPath to
  throwSameFile from' to'
  SD.copyFile from' to'
copyFile _ _ = return ()


-- |Copies the given file to the given dir with the same filename.
-- This can also be called on symlinks.
copyFileToDir :: AnchoredFile FileInfo FileInfo
              -> AnchoredFile FileInfo FileInfo
              -> IO ()
copyFileToDir (IsSymL True) _ = return ()
copyFileToDir from@(_ :/ RegFile fn _)
                to@(_ :/ Dir {}) =
  do
    let from' = fullPath from
        to'   = fullPath to </> fn
    SD.copyFile from' to'
copyFileToDir _ _ = return ()


easyCopy :: DirCopyMode
         -> AnchoredFile FileInfo FileInfo
         -> AnchoredFile FileInfo FileInfo
         -> IO ()
easyCopy _ from@(IsSymL True) to@(_ :/ Dir {}) = recreateSymlink from to
easyCopy _ from@(_ :/ RegFile fn _)
             to@(_ :/ Dir {})
  = copyFileToDir from to
easyCopy _ from@(_ :/ RegFile fn _)
             to@(_ :/ RegFile {})
  = copyFile from to
easyCopy cm from@(_ :/ Dir fn _)
              to@(_ :/ Dir {})
  = copyDir cm from to
easyCopy _ _ _ = return ()



    ---------------------
    --[ File Deletion ]--
    ---------------------


-- |Deletes a symlink, which can either point to a file or directory.
deleteSymlink :: AnchoredFile FileInfo FileInfo -> IO ()
deleteSymlink f@(IsSymL True)
  = removeFile (fullPath f)
deleteSymlink _
  = return ()


-- |Deletes the given file, never symlinks.
deleteFile :: AnchoredFile FileInfo FileInfo -> IO ()
deleteFile   (IsSymL True) = return ()
deleteFile f@(_ :/ RegFile {})
  = removeFile (fullPath f)
deleteFile _
  = return ()


-- |Deletes the given directory, never symlinks.
deleteDir :: AnchoredFile FileInfo FileInfo -> IO ()
deleteDir   (IsSymL True) = return ()
deleteDir f@(_ :/ Dir {})
  = removeDirectory (fullPath f)
deleteDir _ = return ()


-- |Deletes the given directory recursively, never symlinks.
deleteDirRecursive :: AnchoredFile FileInfo FileInfo -> IO ()
deleteDirRecursive (IsSymL True) = return ()
deleteDirRecursive f@(_ :/ Dir {})
  = removeDirectoryRecursive (fullPath f)
deleteDirRecursive _ = return ()


-- |Deletes a file, directory or symlink, whatever it may be.
-- In case of directory, performs recursive deletion.
easyDelete :: AnchoredFile FileInfo FileInfo -> IO ()
easyDelete f@(IsSymL True) = deleteSymlink f
easyDelete f@(_ :/ RegFile {})
  = deleteFile f
easyDelete f@(_ :/ Dir {})
  = deleteDirRecursive f
easyDelete _
  = return ()




    --------------------
    --[ File Opening ]--
    --------------------


-- |Opens a file appropriately by invoking xdg-open.
openFile :: AnchoredFile a b
         -> IO ProcessHandle
openFile f = spawnProcess "xdg-open" [fullPath f]


-- |Executes a program with the given arguments.
executeFile :: AnchoredFile FileInfo FileInfo  -- ^ program
            -> [String]                        -- ^ arguments
            -> IO (Maybe ProcessHandle)
executeFile prog@(_ :/ RegFile _ FileInfo { permissions = perms }) args
  | executable perms = Just <$> spawnProcess (fullPath prog) args
  | otherwise        = return Nothing
executeFile _ _      = return Nothing
