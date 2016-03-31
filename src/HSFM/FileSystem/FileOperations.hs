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

-- |This module provides all the atomic IO related file operations like
-- copy, delete, move and so on. It operates only on FilePaths and reads
-- all necessary file information manually in order to stay atomic and not
-- rely on the state of passed objects.
--
-- It would be nicer to pass states around, but the filesystem state changes
-- too quickly and cannot be relied upon. Lazy implementations of filesystem
-- trees have been tried as well, but they can introduce subtle bugs.
module HSFM.FileSystem.FileOperations where


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
  )
import Data.Foldable
  (
    for_
  )
import Foreign.C.Error
  (
    eXDEV
  )
import HPath
    (
      Path
    , Fn
    )
import qualified HPath as P
import HSFM.FileSystem.Errors
import HSFM.FileSystem.FileType
import HSFM.Utils.IO
import HSFM.Utils.MyPrelude
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
-- TODO: make sure we do the right thing for BlockDev, CharDev etc...
--       most operations are not implemented for these


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
                CopyMode


-- |Data type describing partial or complete file move operation.
-- MC stands for a complete operation and can be used for `runFileOp`.
data Move = MP1 (AnchoredFile FileInfo)
          | MC  (AnchoredFile FileInfo)
                (AnchoredFile FileInfo)
                CopyMode


-- |Copy modes.
data CopyMode = Strict  -- ^ fail if the target already exists
              | Merge   -- ^ overwrite files if necessary, for files, this
                        --   is the same as Replace
              | Replace -- ^ remove targets before copying, this is
                        --   only useful if the target is a directorty


-- |Run a given FileOperation. If the FileOperation is partial, it will
-- be returned.
runFileOp :: FileOperation -> IO (Maybe FileOperation)
runFileOp (FCopy (CC from to cm)) = easyCopy cm from to >> return Nothing
runFileOp (FCopy fo)              = return              $ Just $ FCopy fo
runFileOp (FMove (MC from to cm)) = moveFile cm from to >> return Nothing
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
copyDir :: CopyMode
        -> AnchoredFile FileInfo  -- ^ source dir
        -> AnchoredFile FileInfo  -- ^ destination dir
        -> IO ()
copyDir _ AFileInvFN _ = throw InvalidFileName
copyDir _ _ AFileInvFN = throw InvalidFileName
copyDir cm from@(_ :/ Dir fromn (FileInfo { fileMode = fmode }))
             to@(_ :/ Dir {})
  = do
    let fromp    = fullPath from
        top      = fullPath to
        destdirp = top P.</> fromn
    throwDestinationInSource fromp destdirp
    throwSameFile fromp destdirp

    createDestdir destdirp fmode
    destdir <- HSFM.FileSystem.FileType.readFileWithFileInfo destdirp

    contents <- readDirectoryContents' (fullPath from)

    for_ contents $ \f ->
      case f of
        (_ :/ SymLink {})  -> recreateSymlink cm f destdir
        (_ :/ Dir {})      -> copyDir cm f destdir
        (_ :/ RegFile {})  -> copyFileToDir Replace f destdir
        _                  -> return ()
  where
    createDestdir destdir fmode =
      let destdir' = P.toFilePath destdir
      in case cm of
        Merge   ->
          unlessM (doesDirectoryExist destdir)
                  (createDirectory destdir' fmode)
        Strict  -> do
          throwDirDoesExist destdir
          createDirectory destdir' fmode
        Replace -> do
          whenM (doesDirectoryExist destdir)
                (deleteDirRecursive =<<
                   HSFM.FileSystem.FileType.readFileWithFileInfo destdir)
          createDirectory destdir' fmode
copyDir _ _ _ = throw $ InvalidOperation "wrong input type"


-- |Recreate a symlink.
recreateSymlink :: CopyMode
                -> AnchoredFile FileInfo  -- ^ the old symlink file
                -> AnchoredFile FileInfo  -- ^ destination dir of the
                                          --   new symlink file
                -> IO ()
recreateSymlink _ AFileInvFN _ = throw InvalidFileName
recreateSymlink _ _ AFileInvFN = throw InvalidFileName
recreateSymlink cm    symf@(_ :/ SymLink {})
                   symdest@(_ :/ Dir {})
  = do
    sympoint <- readSymbolicLink (fullPathS $ symf)
    let symname = fullPath symdest P.</> (name . file $ symf)
    case cm of
      Merge   -> delOld symname
      Replace -> delOld symname
      _       -> return ()
    createSymbolicLink sympoint (P.fromAbs symname)
  where
    delOld symname = do
      f <- HSFM.FileSystem.FileType.readFileWithFileInfo symname
      unless (failed . file $ f)
             (easyDelete f)
recreateSymlink _ _ _ = throw $ InvalidOperation "wrong input type"


-- |TODO: handle EAGAIN exception for non-blocking IO
-- |Low-level function to copy a given file to the given path. The fileMode
-- is preserved. The file is always overwritten if accessible.
copyFile' :: FilePath -> FilePath -> IO ()
copyFile' from to = do
  fromFstatus <- getSymbolicLinkStatus from
  fromContent <- BS.readFile from
  fd          <- System.Posix.IO.createFile to
                   (System.Posix.Files.fileMode fromFstatus)
  closeFd fd
  BS.writeFile to fromContent


-- |Copies the given file to the given file destination, overwriting it.
-- Excludes symlinks.
overwriteFile :: AnchoredFile FileInfo  -- ^ source file
              -> AnchoredFile FileInfo  -- ^ destination file
              -> IO ()
overwriteFile AFileInvFN _ = throw InvalidFileName
overwriteFile _ AFileInvFN = throw InvalidFileName
overwriteFile from@(_ :/ RegFile {})
                to@(_ :/ RegFile {})
  = do
    let from' = fullPath from
        to'   = fullPath to
    throwSameFile from' to'
    copyFile' (P.fromAbs from') (P.fromAbs to')
overwriteFile _ _ = throw $ InvalidOperation "wrong input type"


-- |Copies the given file to the given dir with the same filename.
-- Excludes symlinks.
copyFileToDir :: CopyMode
              -> AnchoredFile FileInfo
              -> AnchoredFile FileInfo
              -> IO ()
copyFileToDir _ AFileInvFN _ = throw InvalidFileName
copyFileToDir _ _ AFileInvFN = throw InvalidFileName
copyFileToDir cm from@(_ :/ RegFile fn _)
                   to@(_ :/ Dir {})
  = do
    let from' = fullPath from
        to'   = fullPath to P.</> fn
    case cm of
      Strict -> throwFileDoesExist to'
      _      -> return ()
    copyFile' (P.fromAbs from') (P.fromAbs to')
copyFileToDir _ _ _ = throw $ InvalidOperation "wrong input type"


-- |Copies a file, directory or symlink. In case of a symlink, it is just
-- recreated, even if it points to a directory.
easyCopy :: CopyMode
         -> AnchoredFile FileInfo
         -> AnchoredFile FileInfo
         -> IO ()
easyCopy cm from@(_ :/ SymLink {})
              to@(_ :/ Dir {})
  = recreateSymlink cm from to
easyCopy cm from@(_ :/ RegFile fn _)
              to@(_ :/ Dir {})
  = copyFileToDir cm from to
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
  = removeLink (P.toFilePath . fullPath $ f)
deleteSymlink _ = throw $ InvalidOperation "wrong input type"


-- |Deletes the given file, never symlinks.
deleteFile :: AnchoredFile FileInfo -> IO ()
deleteFile AFileInvFN = throw InvalidFileName
deleteFile f@(_ :/ RegFile {})
  = removeLink (P.toFilePath . fullPath $ f)
deleteFile _ = throw $ InvalidOperation "wrong input type"


-- |Deletes the given directory, never symlinks.
deleteDir :: AnchoredFile FileInfo -> IO ()
deleteDir AFileInvFN = throw InvalidFileName
deleteDir f@(_ :/ Dir {})
  = removeDirectory (P.toFilePath . fullPath $ f)
deleteDir _ = throw $ InvalidOperation "wrong input type"


-- TODO: check if we have permissions at all to remove the directory,
--       before we go recursively messing with it
-- |Deletes the given directory recursively.
deleteDirRecursive :: AnchoredFile FileInfo -> IO ()
deleteDirRecursive AFileInvFN = throw InvalidFileName
deleteDirRecursive f@(_ :/ Dir {}) = do
  let fp = fullPath f
  files <- readDirectoryContents' fp
  for_ files $ \file ->
    case file of
      (_ :/ SymLink {}) -> deleteSymlink file
      (_ :/ Dir {})     -> deleteDirRecursive file
      (_ :/ RegFile {}) -> removeLink (P.toFilePath . fullPath $ file)
      _                 -> throw $ FileDoesExist (P.toFilePath . fullPath $ file)
  removeDirectory . P.toFilePath $ fp
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
openFile f = spawnProcess "xdg-open" [fullPathS f]


-- |Executes a program with the given arguments.
executeFile :: AnchoredFile FileInfo  -- ^ program
            -> [String]               -- ^ arguments
            -> IO ProcessHandle
executeFile AFileInvFN _ = throw InvalidFileName
executeFile prog@(_ :/ RegFile {}) args
  = spawnProcess (fullPathS prog) args
executeFile _ _ = throw $ InvalidOperation "wrong input type"




    ---------------------
    --[ File Creation ]--
    ---------------------


createFile :: AnchoredFile FileInfo -> Path Fn -> IO ()
createFile AFileInvFN _ = throw InvalidFileName
createFile _ InvFN      = throw InvalidFileName
createFile (ADirOrSym td) (ValFN fn) = do
  let fullp = fullPath td P.</> fn
  throwFileDoesExist fullp
  fd <- System.Posix.IO.createFile (P.fromAbs fullp) newFilePerms
  closeFd fd
createFile _ _ = throw $ InvalidOperation "wrong input type"


createDir :: AnchoredFile FileInfo -> Path Fn -> IO ()
createDir AFileInvFN _ = throw InvalidFileName
createDir _ InvFN      = throw InvalidFileName
createDir (ADirOrSym td) (ValFN fn) = do
  let fullp = fullPath td P.</> fn
  throwDirDoesExist fullp
  createDirectory (P.fromAbs fullp) newFilePerms
createDir _ _ = throw $ InvalidOperation "wrong input type"




    ----------------------------
    --[ File Renaming/Moving ]--
    ----------------------------


renameFile :: AnchoredFile FileInfo -> Path Fn -> IO ()
renameFile AFileInvFN _ = throw InvalidFileName
renameFile _ InvFN      = throw InvalidFileName
renameFile af (ValFN fn) = do
  let fromf = fullPath af
      tof   = anchor af P.</> fn
  throwFileDoesExist tof
  throwSameFile fromf tof
  rename (P.fromAbs fromf) (P.fromAbs tof)
renameFile _ _ = throw $ InvalidOperation "wrong input type"


-- |Move a given file to the given target directory.
moveFile :: CopyMode
         -> AnchoredFile FileInfo -- ^ file to move
         -> AnchoredFile FileInfo -- ^ base target directory
         -> IO ()
moveFile _ AFileInvFN _ = throw InvalidFileName
moveFile _ _ AFileInvFN = throw InvalidFileName
moveFile cm from to@(_ :/ Dir {}) = do
  let from'  = fullPath from
      froms' = fullPathS from
      to'    = fullPath to P.</> (name . file $ from)
      tos'   = P.fromAbs (fullPath to P.</> (name . file $ from))
  case cm of
    Strict  -> throwFileDoesExist to'
    Merge   -> delOld to'
    Replace -> delOld to'
  throwSameFile from' to'
  catchErrno eXDEV (rename froms' tos') $ do
    easyCopy Strict from to
    easyDelete from
  where
    delOld fp = do
      to' <- HSFM.FileSystem.FileType.readFileWithFileInfo fp
      unless (failed . file $ to') (easyDelete to')
moveFile _ _ _ = throw $ InvalidOperation "wrong input type"





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
