{--
HSFM, a filemanager written in Haskell.
Copyright (C) 2015 Julian Ospald

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
import Data.DirTree
import Data.Foldable
  (
    for_
  )
import Foreign.C.Error
  (
    eXDEV
  )
import IO.Error
import IO.Utils
import System.FilePath
  (
    (</>)
  )
import System.Posix.Directory
  (
    createDirectory
  , removeDirectory
  )
import System.Posix.Files
  (
    createSymbolicLink
  , fileMode
  , readSymbolicLink
  , getSymbolicLinkStatus
  , groupExecuteMode
  , groupReadMode
  , groupWriteMode
  , otherExecuteMode
  , otherReadMode
  , otherWriteMode
  , ownerModes
  , ownerReadMode
  , ownerWriteMode
  , rename
  , unionFileModes
  , removeLink
  )
import System.Posix.IO
  (
    closeFd
  , createFile
  )
import System.Posix.Types
  (
    FileMode
  )
import System.Process
  (
    spawnProcess
  , ProcessHandle
  )

import qualified Data.ByteString  as BS



-- TODO: file operations should be threaded and not block the UI


-- |Data type describing an actual file operation that can be
-- carried out via `doFile`. Useful to build up a list of operations
-- or delay operations.
data FileOperation = FCopy    Copy
                   | FMove    Move
                   | FDelete  (AnchoredFile FileInfo)
                   | FOpen    (AnchoredFile FileInfo)
                   | FExecute (AnchoredFile FileInfo) [String]
                   | None


-- |Data type describing partial or complete file copy operation.
-- CC stands for a complete operation and can be used for `runFileOp`.
data Copy = CP1 (AnchoredFile FileInfo)
          | CP2 (AnchoredFile FileInfo)
                (AnchoredFile FileInfo)
          | CC  (AnchoredFile FileInfo)
                (AnchoredFile FileInfo)
                DirCopyMode


-- |Data type describing partial or complete file move operation.
-- MC stands for a complete operation and can be used for `runFileOp`.
data Move = MP1 (AnchoredFile FileInfo)
          | MC  (AnchoredFile FileInfo)
                (AnchoredFile FileInfo)


-- |Directory copy modes.
data DirCopyMode = Strict  -- ^ fail if the target directory already exists
                 | Merge   -- ^ overwrite files if necessary
                 | Replace -- ^ remove target directory before copying


-- |Run a given FileOperation. If the FileOperation is partial, it will
-- be returned.
runFileOp :: FileOperation -> IO (Maybe FileOperation)
runFileOp (FCopy (CC from to cm)) = easyCopy cm from to >> return Nothing
runFileOp (FCopy fo)              = return              $ Just $ FCopy fo
runFileOp (FMove (MC from to))    = moveFile from to    >> return Nothing
runFileOp (FMove fo)              = return              $ Just $ FMove fo
runFileOp (FDelete fp)            = easyDelete fp       >> return Nothing
runFileOp (FOpen fp)              = openFile fp         >> return Nothing
runFileOp (FExecute fp args)      = executeFile fp args >> return Nothing



    --------------------
    --[ File Copying ]--
    --------------------


-- TODO: allow renaming
-- |Copies a directory to the given destination with the specified
-- `DirCopyMode`. Excludes symlinks.
copyDir :: DirCopyMode
        -> AnchoredFile FileInfo  -- ^ source dir
        -> AnchoredFile FileInfo  -- ^ destination dir
        -> IO ()
copyDir _ AFileInvFN _ = throw InvalidFileName
copyDir _ _ AFileInvFN = throw InvalidFileName
copyDir cm from@(_ :/ Dir fromn _)
             to@(_ :/ Dir {})
  = do
    let fromp    = fullPath from
        top      = fullPath to
        destdirp = top </> fromn
    throwDestinationInSource fromp destdirp
    throwSameFile fromp destdirp

    createDestdir destdirp
    destdir <- Data.DirTree.readFile destdirp

    contents <- readDirectory' (fullPath from)

    for_ contents $ \f ->
      case f of
        (_ :/ SymLink {})  -> recreateSymlink f destdir
        (_ :/ Dir {}) -> copyDir cm f destdir
        (_ :/ RegFile {}) -> copyFileToDir f destdir
        _                 -> return ()
  where
    createDestdir destdir =
      case cm of
        Merge   ->
          unlessM (doesDirectoryExist destdir)
                  (createDirectory destdir newDirPerms)
        Strict  -> do
          throwDirDoesExist destdir
          createDirectory destdir newDirPerms
        Replace -> do
          whenM (doesDirectoryExist destdir)
                (deleteDirRecursive =<< Data.DirTree.readFile destdir)
          createDirectory destdir newDirPerms
    recreateSymlink' f destdir = do
      let destfilep = fullPath destdir </> (name . file $ f)
      destfile <- Data.DirTree.readFile destfilep

      _ <- case cm of
        -- delete old file/dir to be able to create symlink
        Merge -> easyDelete destfile
        _     -> return ()

      recreateSymlink f destdir
copyDir _ _ _ = throw $ InvalidOperation "wrong input type"


-- |Recreate a symlink.
recreateSymlink :: AnchoredFile FileInfo  -- ^ the old symlink file
                -> AnchoredFile FileInfo  -- ^ destination dir of the
                                                   --   new symlink file
                -> IO ()
recreateSymlink AFileInvFN _ = throw InvalidFileName
recreateSymlink _ AFileInvFN = throw InvalidFileName
recreateSymlink symf@(_ :/ SymLink {})
                symdest@(_ :/ Dir {})
  = do
    symname <- readSymbolicLink (fullPath symf)
    createSymbolicLink symname (fullPath symdest </> (name . file $ symf))
recreateSymlink _ _ = throw $ InvalidOperation "wrong input type"


-- |TODO: handle EAGAIN exception for non-blocking IO
-- |Low-level function to copy a given file to the given path. The fileMode
-- is preserved.
copyFile' :: FilePath -> FilePath -> IO ()
copyFile' InvFN _ = throw InvalidFileName
copyFile' _ InvFN = throw InvalidFileName
copyFile' from to = do
  fromFstatus <- getSymbolicLinkStatus from
  fromContent <- BS.readFile from
  fd          <- System.Posix.IO.createFile to
                   (System.Posix.Files.fileMode fromFstatus)
  closeFd fd
  BS.writeFile to fromContent


-- |Copies the given file to the given file destination.
-- Excludes symlinks.
copyFile :: AnchoredFile FileInfo  -- ^ source file
         -> AnchoredFile FileInfo  -- ^ destination file
         -> IO ()
copyFile AFileInvFN _ = throw InvalidFileName
copyFile _ AFileInvFN = throw InvalidFileName
copyFile (_ :/ SymLink {}) _ = return ()
copyFile from@(_ :/ RegFile {}) to@(_ :/ RegFile {}) = do
  let from' = fullPath from
      to'   = fullPath to
  throwSameFile from' to'
  copyFile' from' to'
copyFile _ _ = throw $ InvalidOperation "wrong input type"


-- |Copies the given file to the given dir with the same filename.
-- Excludes symlinks.
copyFileToDir :: AnchoredFile FileInfo
              -> AnchoredFile FileInfo
              -> IO ()
copyFileToDir AFileInvFN _ = throw InvalidFileName
copyFileToDir _ AFileInvFN = throw InvalidFileName
copyFileToDir (_ :/ SymLink {}) _ = return ()
copyFileToDir from@(_ :/ RegFile fn _)
                to@(_ :/ Dir {}) =
  do
    let from' = fullPath from
        to'   = fullPath to </> fn
    copyFile' from' to'
copyFileToDir _ _ = throw $ InvalidOperation "wrong input type"


-- |Copies a file, directory or symlink. In case of a symlink, it is just
-- recreated, even if it points to a directory.
easyCopy :: DirCopyMode
         -> AnchoredFile FileInfo
         -> AnchoredFile FileInfo
         -> IO ()
easyCopy _ from@(_ :/ SymLink {}) to@(_ :/ Dir {}) = recreateSymlink from to
easyCopy _ from@(_ :/ RegFile fn _)
             to@(_ :/ Dir {})
  = copyFileToDir from to
easyCopy _ from@(_ :/ RegFile fn _)
             to@(_ :/ RegFile {})
  = copyFile from to
easyCopy cm from@(_ :/ Dir fn _)
              to@(_ :/ Dir {})
  = copyDir cm from to
easyCopy _ _ _ = throw $ InvalidOperation "wrong input type"





    ---------------------
    --[ File Deletion ]--
    ---------------------


-- |Deletes a symlink, which can either point to a file or directory.
deleteSymlink :: AnchoredFile FileInfo -> IO ()
deleteSymlink AFileInvFN = throw InvalidFileName
deleteSymlink f@(_ :/ SymLink {})
  = removeLink (fullPath f)
deleteSymlink _ = throw $ InvalidOperation "wrong input type"


-- |Deletes the given file, never symlinks.
deleteFile :: AnchoredFile FileInfo -> IO ()
deleteFile AFileInvFN = throw InvalidFileName
deleteFile   (_ :/ SymLink {}) = return ()
deleteFile f@(_ :/ RegFile {})
  = removeLink (fullPath f)
deleteFile _ = throw $ InvalidOperation "wrong input type"


-- |Deletes the given directory, never symlinks.
deleteDir :: AnchoredFile FileInfo -> IO ()
deleteDir AFileInvFN = throw InvalidFileName
deleteDir   (_ :/ SymLink {}) = return ()
deleteDir f@(_ :/ Dir {})
  = removeDirectory (fullPath f)
deleteDir _ = throw $ InvalidOperation "wrong input type"


-- |Deletes the given directory recursively.
deleteDirRecursive :: AnchoredFile FileInfo -> IO ()
deleteDirRecursive AFileInvFN = throw InvalidFileName
deleteDirRecursive f@(_ :/ Dir {}) = do
  let fp = fullPath f
  files <- readDirectory' fp
  for_ files $ \file ->
    case file of
      (_ :/ SymLink {}) -> deleteSymlink file
      (_ :/ Dir {})     -> deleteDirRecursive file
      (AFileLike _)     -> removeLink (fullPath file)
      _                 -> throw $ FileDoesExist (fullPath file)
  removeDirectory fp
deleteDirRecursive _ = throw $ InvalidOperation "wrong input type"


-- |Deletes a file, directory or symlink, whatever it may be.
-- In case of directory, performs recursive deletion. In case of
-- a symlink, the symlink file is deleted.
easyDelete :: AnchoredFile FileInfo -> IO ()
easyDelete f@(_ :/ SymLink {}) = deleteSymlink f
easyDelete f@(_ :/ RegFile {})
  = deleteFile f
easyDelete f@(_ :/ Dir {})
  = deleteDirRecursive f
easyDelete _ = throw $ InvalidOperation "wrong input type"




    --------------------
    --[ File Opening ]--
    --------------------


-- |Opens a file appropriately by invoking xdg-open.
openFile :: AnchoredFile a
         -> IO ProcessHandle
openFile AFileInvFN = throw InvalidFileName
openFile f = spawnProcess "xdg-open" [fullPath f]


-- |Executes a program with the given arguments.
executeFile :: AnchoredFile FileInfo  -- ^ program
            -> [String]               -- ^ arguments
            -> IO ProcessHandle
executeFile AFileInvFN _ = throw InvalidFileName
executeFile prog@(_ :/ RegFile {}) args
  = spawnProcess (fullPath prog) args
executeFile _ _ = throw $ InvalidOperation "wrong input type"




    ---------------------
    --[ File Creation ]--
    ---------------------


createFile :: AnchoredFile FileInfo -> FileName -> IO ()
createFile AFileInvFN _ = throw InvalidFileName
createFile _ InvFN      = throw InvalidFileName
createFile (ADirOrSym td) (ValFN fn) = do
  let fullp = fullPath td </> fn
  throwFileDoesExist fullp
  fd <- System.Posix.IO.createFile fullp newFilePerms
  closeFd fd
createFile _ _ = throw $ InvalidOperation "wrong input type"


createDir :: AnchoredFile FileInfo -> FileName -> IO ()
createDir AFileInvFN _ = throw InvalidFileName
createDir _ InvFN      = throw InvalidFileName
createDir (ADirOrSym td) (ValFN fn) = do
  let fullp = fullPath td </> fn
  throwDirDoesExist fullp
  createDirectory fullp newFilePerms
createDir _ _ = throw $ InvalidOperation "wrong input type"




    ----------------------------
    --[ File Renaming/Moving ]--
    ----------------------------


renameFile :: AnchoredFile FileInfo -> FileName -> IO ()
renameFile AFileInvFN _ = throw InvalidFileName
renameFile _ InvFN      = throw InvalidFileName
renameFile (_ :/ Failed {}) _ = return ()
renameFile af (ValFN fn) = do
  let fromf = fullPath af
      tof   = anchor af </> fn
  throwFileDoesExist tof
  throwSameFile fromf tof
  rename fromf tof
renameFile _ _ = throw $ InvalidOperation "wrong input type"


-- |Move a given file to the given target directory.
moveFile :: AnchoredFile FileInfo -- ^ file to move
         -> AnchoredFile FileInfo -- ^ base target directory
         -> IO ()
moveFile AFileInvFN _ = throw InvalidFileName
moveFile _ AFileInvFN = throw InvalidFileName
moveFile from to@(_ :/ Dir {}) = do
  let from' = fullPath from
      to'   = fullPath to </> (name . file $ from)
  throwFileDoesExist to'
  throwSameFile from' to'
  catchErrno eXDEV (rename from' to') $ do
    easyCopy Strict from to
    easyDelete from
moveFile _ _ = throw $ InvalidOperation "wrong input type"





    -----------------------
    --[ File Permissions]--
    -----------------------


newFilePerms :: FileMode
newFilePerms
  =                  ownerWriteMode
    `unionFileModes` ownerReadMode
    `unionFileModes` groupWriteMode
    `unionFileModes` groupReadMode
    `unionFileModes` otherWriteMode
    `unionFileModes` otherReadMode


newDirPerms :: FileMode
newDirPerms
  =                  ownerModes
    `unionFileModes` groupExecuteMode
    `unionFileModes` groupReadMode
    `unionFileModes` otherExecuteMode
    `unionFileModes` otherReadMode
