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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

-- |This module provides all the atomic IO related file operations like
-- copy, delete, move and so on. It operates primarily on `AnchoredFile`, which
-- is guaranteed to be well-formed.
--
-- It would be nicer to pass states around, but the filesystem state changes
-- too quickly and cannot be relied upon. Lazy implementations of filesystem
-- trees have been tried as well, but they can introduce subtle bugs.
module HSFM.FileSystem.FileOperations where


import Control.Exception
  (
    bracket
  , throw
  )
import Control.Monad
  (
    unless
  , when
  , void
  )
import Data.ByteString
  (
    ByteString
  )
import Data.Foldable
  (
    for_
  )
import Data.Word
  (
    Word8
  )
import Foreign.C.Error
  (
    eXDEV
  , eINVAL
  , eNOSYS
  )
import Foreign.Marshal.Alloc
  (
    allocaBytes
  )
import Foreign.Ptr
  (
    Ptr
  )
import HPath
  (
    Path
  , Abs
  , Fn
  )
import qualified HPath as P
import HSFM.FileSystem.Errors
import HSFM.FileSystem.FileType
import HSFM.Utils.IO
import Prelude hiding (readFile)
import Network.Sendfile
  (
    sendfileFd
  , FileRange(EntireFile)
  )
import System.Posix.Directory.ByteString
  (
    createDirectory
  , removeDirectory
  )
import System.Posix.Files.ByteString
  (
    createSymbolicLink
  , readSymbolicLink
  , fileMode
  , getFdStatus
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
import qualified System.Posix.Files.ByteString as PF
import qualified "unix" System.Posix.IO.ByteString as SPI
import qualified "unix-bytestring" System.Posix.IO.ByteString as SPB
import qualified System.Posix.Process.ByteString as SPP
import System.Posix.Types
  (
    FileMode
  , ProcessID
  , Fd
  )



-- TODO: file operations should be threaded and not block the UI
-- TODO: make sure we do the right thing for BlockDev, CharDev etc...
--       most operations are not implemented for these


-- |Data type describing an actual file operation that can be
-- carried out via `doFile`. Useful to build up a list of operations
-- or delay operations.
data FileOperation = FCopy    Copy
                   | FMove    Move
                   | FDelete  [Path Abs]
                   | FOpen    (Path Abs)
                   | FExecute (Path Abs) [ByteString]
                   | None


-- |Data type describing partial or complete file copy operation.
-- CC stands for a complete operation and can be used for `runFileOp`.
data Copy = CP1 [Path Abs]
          | CP2 [Path Abs]
                (Path Abs)
          | CC  [Path Abs]
                (Path Abs)
                CopyMode


-- |Data type describing partial or complete file move operation.
-- MC stands for a complete operation and can be used for `runFileOp`.
data Move = MP1 [Path Abs]
          | MC  [Path Abs]
                (Path Abs)
                CopyMode


-- |Copy modes.
data CopyMode = Strict  -- ^ fail if the target already exists
              | Merge   -- ^ overwrite files if necessary, for files, this
                        --   is the same as Replace
              | Replace -- ^ remove targets before copying, this is
                        --   only useful if the target is a directorty
              | Rename (Path Fn)


-- |Run a given FileOperation. If the FileOperation is partial, it will
-- be returned. Returns `Nothing` on success.
--
-- Since file operations can be delayed, this is `Path Abs` based, not
-- `AnchoredFile` based. This makes sure we don't have stale
-- file information.
runFileOp :: FileOperation -> IO (Maybe FileOperation)
runFileOp fo' =
  case fo' of
    (FCopy (CC froms to cm)) -> do
      froms' <- mapM toAfile froms
      to'    <- toAfile to
      when (anyFailed $ file <$> froms')
           (throw . CopyFailed $ "File in copy buffer does not exist anymore!")
      mapM_ (\x -> easyCopy cm x to') froms'
        >> return Nothing
    (FCopy fo) -> return $ Just $ FCopy fo
    (FMove (MC froms to cm)) -> do
      froms' <- mapM toAfile froms
      to'   <- toAfile to
      when (anyFailed $ file <$> froms')
           (throw . MoveFailed $ "File in move buffer does not exist anymore!")
      mapM_ (\x -> easyMove cm x to') froms'
        >> return Nothing
    (FMove fo) -> return $ Just $ FMove fo
    (FDelete fps) -> do
      fps' <- mapM toAfile fps
      mapM_ easyDelete fps' >> return Nothing
    (FOpen fp) ->
      toAfile fp >>= openFile >> return Nothing
    (FExecute fp args) ->
      toAfile fp >>= flip executeFile args >> return Nothing
    _ -> return Nothing
  where
    toAfile = readFile (\_ -> return undefined)



    --------------------
    --[ File Copying ]--
    --------------------


-- |Copies a directory to the given destination with the specified
-- `DirCopyMode`. Excludes symlinks.
copyDir :: CopyMode
        -> AnchoredFile a  -- ^ source dir
        -> AnchoredFile a  -- ^ destination dir
        -> Path Fn         -- ^ destination dir name
        -> IO ()
copyDir _ AFileInvFN _ _ = throw InvalidFileName
copyDir _ _ AFileInvFN _ = throw InvalidFileName
copyDir _ _ _ InvFN      = throw InvalidFileName
copyDir       (Rename fn)
         from@(_ :/ Dir {})
           to@(_ :/ Dir {})
           _
  = copyDir Strict from to fn
-- this branch must never get `Rename` as CopyMode
copyDir cm from@(_ :/ Dir {})
             to@(_ :/ Dir {})
             fn
  = do
    let fromp    = fullPath from
        top      = fullPath to
        destdirp = top P.</> fn
    -- for performance, sanity checks are only done for the top dir
    throwDestinationInSource fromp destdirp
    throwSameFile fromp destdirp
    throwCantOpenDirectory fromp
    throwCantOpenDirectory top
    go cm from to fn
  where
    go :: CopyMode -> AnchoredFile a -> AnchoredFile a -> Path Fn -> IO ()
    go cm' from'@(_ :/ Dir {})
             to'@(_ :/ Dir {})
             fn' = do
      fmode' <- PF.fileMode <$> PF.getSymbolicLinkStatus (fullPathS from')
      createDestdir (fullPath to' P.</> fn') fmode'
      destdir <- readFileUnsafe (\_ -> return undefined)
                   (fullPath to' P.</> fn')
      contents <- readDirectoryContentsUnsafe
                    getDirsFiles (\_ -> return undefined) (fullPath from')

      for_ contents $ \f ->
        case f of
          (_ :/ SymLink {})  -> recreateSymlink cm' f destdir (name . file $ f)
          (_ :/ Dir {})      -> go cm' f destdir (name . file $ f)
          (_ :/ RegFile {})  -> unsafeCopyFile Replace f destdir
                                               (name . file $ f)
          _                  -> return ()
      where
        createDestdir destdir fmode' =
          let destdir' = P.toFilePath destdir
          in case cm' of
            Merge   ->
              unlessM (doesDirectoryExist destdir)
                      (createDirectory destdir' fmode')
            Strict  -> do
              throwDirDoesExist destdir
              createDirectory destdir' fmode'
            Replace -> do
              whenM (doesDirectoryExist destdir)
                    (deleteDirRecursive =<<
                       readFileUnsafe
                         (\_ -> return undefined) destdir)
              createDirectory destdir' fmode'
            _ -> throw $ InvalidOperation "Internal error, wrong CopyMode!"
    go _ _ _ _ = throw $ InvalidOperation "wrong input type"
copyDir _ _ _ _ = throw $ InvalidOperation "wrong input type"


-- |Recreate a symlink.
recreateSymlink :: CopyMode
                -> AnchoredFile a  -- ^ the old symlink file
                -> AnchoredFile a  -- ^ destination dir of the
                                   --   new symlink file
                -> Path Fn         -- ^ destination file name
                -> IO ()
recreateSymlink _ AFileInvFN _ _ = throw InvalidFileName
recreateSymlink _ _ AFileInvFN _ = throw InvalidFileName
recreateSymlink _ _ _ InvFN      = throw InvalidFileName
recreateSymlink (Rename pn) symf@(_ :/ SymLink {}) symdest@(_ :/ Dir {}) _
  = recreateSymlink Strict symf symdest pn
recreateSymlink cm symf@(_ :/ SymLink {}) symdest@(_ :/ Dir {}) fn
  = do
    throwCantOpenDirectory $ fullPath symdest
    sympoint <- readSymbolicLink (fullPathS $ symf)
    let symname = fullPath symdest P.</> fn
    case cm of
      Merge   -> delOld symname
      Replace -> delOld symname
      _       -> return ()
    createSymbolicLink sympoint (P.fromAbs symname)
  where
    delOld symname = do
      f <- readFileUnsafe (\_ -> return undefined) symname
      unless (failed . file $ f)
             (easyDelete f)
recreateSymlink _ _ _ _ = throw $ InvalidOperation "wrong input type"


-- |TODO: handle EAGAIN exception for non-blocking IO
-- |Copies the given regular file to the given dir with the given filename.
-- Excludes symlinks.
copyFile :: CopyMode
         -> AnchoredFile a  -- ^ source file
         -> AnchoredFile a  -- ^ destination dir
         -> Path Fn         -- ^ destination file name
         -> IO ()
copyFile _ AFileInvFN _ _ = throw InvalidFileName
copyFile _ _ AFileInvFN _ = throw InvalidFileName
copyFile _ _ _ InvFN      = throw InvalidFileName
copyFile (Rename pn) from@(_ :/ RegFile {}) to@(_ :/ Dir {}) _
  = copyFile Strict from to pn
copyFile cm from@(_ :/ RegFile {}) to@(_ :/ Dir {}) fn
  = do
    let to'   = fullPath to P.</> fn
    throwCantOpenDirectory $ fullPath to
    throwCantOpenDirectory . P.dirname . fullPath $ from
    throwSameFile (fullPath from) to'
    unsafeCopyFile cm from to fn
copyFile _ _ _ _ = throw $ InvalidOperation "wrong input type"


-- |Unsafe version of `copyFile` without initial sanity checks. Thise
-- holds the actual copy logic though and is called by `copyFile` in the end.
unsafeCopyFile :: CopyMode
               -> AnchoredFile a  -- ^ source file
               -> AnchoredFile a  -- ^ destination dir
               -> Path Fn         -- ^ destination file name
               -> IO ()
unsafeCopyFile _ AFileInvFN _ _ = throw InvalidFileName
unsafeCopyFile _ _ AFileInvFN _ = throw InvalidFileName
unsafeCopyFile _ _ _ InvFN      = throw InvalidFileName
unsafeCopyFile (Rename pn) from@(_ :/ RegFile {}) to@(_ :/ Dir {}) _
  = copyFile Strict from to pn
unsafeCopyFile cm from@(_ :/ RegFile {}) to@(_ :/ Dir {}) fn
  = do
    let to'   = fullPath to P.</> fn
    case cm of
      Strict -> throwFileDoesExist to'
      _      -> return ()

    -- from sendfile(2) manpage:
    --   Applications  may  wish  to  fall back to read(2)/write(2) in the case
    --   where sendfile() fails with EINVAL or ENOSYS.
    catchErrno [eINVAL, eNOSYS]
               (sendFileCopy (fullPathS from) (P.fromAbs to'))
               (void $ fallbackCopy (fullPathS from) (P.fromAbs to'))
  where
    -- this is low-level stuff utilizing sendfile(2) for speed
    -- TODO: preserve permissions
    sendFileCopy source dest =
      bracket (SPI.openFd source SPI.ReadOnly Nothing SPI.defaultFileFlags)
              SPI.closeFd
              $ \sfd -> do
                fileM <- System.Posix.Files.ByteString.fileMode
                         <$> getFdStatus sfd
                bracket (SPI.openFd dest SPI.WriteOnly (Just fileM)
                                    SPI.defaultFileFlags)
                        SPI.closeFd
                        $ \dfd -> sendfileFd dfd sfd EntireFile (return ())
    -- low-level copy operation utilizing read(2)/write(2)
    -- in case `sendFileCopy` fails/is unsupported
    fallbackCopy source dest =
      bracket (SPI.openFd source SPI.ReadOnly Nothing SPI.defaultFileFlags)
              SPI.closeFd
              $ \sfd -> do
                fileM <- System.Posix.Files.ByteString.fileMode
                         <$> getFdStatus sfd
                bracket (SPI.openFd dest SPI.WriteOnly (Just fileM)
                                    SPI.defaultFileFlags)
                        SPI.closeFd
                        $ \dfd -> allocaBytes 8192 $ \buf ->
                                    write' sfd dfd buf 0
      where
        write' :: Fd -> Fd -> Ptr Word8 -> Int -> IO Int
        write' sfd dfd buf totalsize = do
            size <- SPB.fdReadBuf sfd buf 8192
            if (size == 0)
              then return $ fromIntegral totalsize
              else do rsize <- SPB.fdWriteBuf dfd buf size
                      when (rsize /= size) (throw . CopyFailed $ "wrong size!")
                      write' sfd dfd buf (totalsize + fromIntegral size)
unsafeCopyFile _ _ _ _ = throw $ InvalidOperation "wrong input type"


-- |Copies a regular file, directory or symlink. In case of a symlink,
-- it is just recreated, even if it points to a directory.
easyCopy :: CopyMode
         -> AnchoredFile a
         -> AnchoredFile a
         -> IO ()
easyCopy cm from@(_ :/ SymLink{})
              to@(_ :/ Dir{})
  = recreateSymlink cm from to (name . file $ from)
easyCopy cm from@(_ :/ RegFile{})
              to@(_ :/ Dir{})
  = copyFile cm from to (name . file $ from)
easyCopy cm from@(_ :/ Dir{})
              to@(_ :/ Dir{})
  = copyDir cm from to (name . file $ from)
easyCopy _ _ _ = throw $ InvalidOperation "wrong input type"





    ---------------------
    --[ File Deletion ]--
    ---------------------


-- |Deletes a symlink, which can either point to a file or directory.
deleteSymlink :: AnchoredFile a -> IO ()
deleteSymlink AFileInvFN = throw InvalidFileName
deleteSymlink f@(_ :/ SymLink {})
  = removeLink (P.toFilePath . fullPath $ f)
deleteSymlink _ = throw $ InvalidOperation "wrong input type"


-- |Deletes the given regular file, never symlinks.
deleteFile :: AnchoredFile a -> IO ()
deleteFile AFileInvFN = throw InvalidFileName
deleteFile f@(_ :/ RegFile {})
  = removeLink (P.toFilePath . fullPath $ f)
deleteFile _ = throw $ InvalidOperation "wrong input type"


-- |Deletes the given directory, never symlinks.
deleteDir :: AnchoredFile a -> IO ()
deleteDir AFileInvFN = throw InvalidFileName
deleteDir f@(_ :/ Dir {})
  = removeDirectory (P.toFilePath . fullPath $ f)
deleteDir _ = throw $ InvalidOperation "wrong input type"


-- |Deletes the given directory recursively.
deleteDirRecursive :: AnchoredFile a -> IO ()
deleteDirRecursive AFileInvFN = throw InvalidFileName
deleteDirRecursive f'@(_ :/ Dir {}) = do
  let fp = fullPath f'
  throwCantOpenDirectory fp
  go f'
  where
    go :: AnchoredFile a -> IO ()
    go f@(_ :/ Dir {}) = do
      let fp = fullPath f
      files <- readDirectoryContentsUnsafe getDirsFiles
                 (\_ -> return undefined) fp
      for_ files $ \file ->
        case file of
          (_ :/ SymLink {}) -> deleteSymlink file
          (_ :/ Dir {})     -> go file
          (_ :/ RegFile {}) -> removeLink (P.toFilePath . fullPath $ file)
          _                 -> throw $ FileDoesExist
                                       (P.fpToString . P.toFilePath . fullPath
                                                     $ file)
      removeDirectory . P.toFilePath $ fp
    go _ = throw $ InvalidOperation "wrong input type"
deleteDirRecursive _ = throw $ InvalidOperation "wrong input type"


-- |Deletes a file, directory or symlink, whatever it may be.
-- In case of directory, performs recursive deletion. In case of
-- a symlink, the symlink file is deleted.
easyDelete :: AnchoredFile a -> IO ()
easyDelete f@(_ :/ SymLink {}) = deleteSymlink f
easyDelete f@(_ :/ RegFile {})
  = deleteFile f
easyDelete f@(_ :/ Dir {})
  = deleteDirRecursive f
easyDelete _ = throw $ InvalidOperation "wrong input type"




    --------------------
    --[ File Opening ]--
    --------------------


-- |Opens a file appropriately by invoking xdg-open. The file type
-- is not checked.
openFile :: AnchoredFile a
         -> IO ProcessID
openFile AFileInvFN = throw InvalidFileName
openFile f =
  SPP.forkProcess $ SPP.executeFile "xdg-open" True [fullPathS f] Nothing


-- |Executes a program with the given arguments.
executeFile :: AnchoredFile a  -- ^ program
            -> [ByteString]    -- ^ arguments
            -> IO ProcessID
executeFile AFileInvFN _ = throw InvalidFileName
executeFile prog@(_ :/ RegFile {}) args
  = SPP.forkProcess $ SPP.executeFile (fullPathS prog) True args Nothing
executeFile prog@(_ :/ SymLink { sdest = (_ :/ RegFile {}) }) args
  = SPP.forkProcess $ SPP.executeFile (fullPathS prog) True args Nothing
executeFile _ _ = throw $ InvalidOperation "wrong input type"




    ---------------------
    --[ File Creation ]--
    ---------------------


-- |Create an empty regular file at the given directory with the given filename.
createFile :: AnchoredFile FileInfo -> Path Fn -> IO ()
createFile AFileInvFN _ = throw InvalidFileName
createFile _ InvFN      = throw InvalidFileName
createFile (ADirOrSym td) (ValFN fn) = do
  let fullp = fullPath td P.</> fn
  throwFileDoesExist fullp
  fd <- SPI.createFile (P.fromAbs fullp) newFilePerms
  SPI.closeFd fd
createFile _ _ = throw $ InvalidOperation "wrong input type"


-- |Create an empty directory at the given directory with the given filename.
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


-- |Rename a given file with the provided filename.
renameFile :: AnchoredFile a -> Path Fn -> IO ()
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
         -> AnchoredFile a -- ^ file to move
         -> AnchoredFile a -- ^ base target directory
         -> Path Fn        -- ^ target file name
         -> IO ()
moveFile _ AFileInvFN _ _ = throw InvalidFileName
moveFile _ _ AFileInvFN _ = throw InvalidFileName
moveFile (Rename pn) from to@(_ :/ Dir {}) _ =
  moveFile Strict from to pn
moveFile cm from to@(_ :/ Dir {}) fn = do
  let from'  = fullPath from
      froms' = fullPathS from
      to'    = fullPath to P.</> fn
      tos'   = P.fromAbs (fullPath to P.</> fn)
  case cm of
    Strict   -> throwFileDoesExist to'
    Merge    -> delOld to'
    Replace  -> delOld to'
    Rename _ -> throw $ InvalidOperation "Internal error! Wrong CopyMode!"
  throwSameFile from' to'
  catchErrno [eXDEV] (rename froms' tos') $ do
    easyCopy Strict from to
    easyDelete from
  where
    delOld fp = do
      to' <- readFileUnsafe (\_ -> return undefined) fp
      unless (failed . file $ to') (easyDelete to')
moveFile _ _ _ _ = throw $ InvalidOperation "wrong input type"


-- |Like `moveFile` except it uses the filename of the source as target.
easyMove :: CopyMode
         -> AnchoredFile a -- ^ file to move
         -> AnchoredFile a -- ^ base target directory
         -> IO ()
easyMove cm from to = moveFile cm from to (name . file $ from)



    -----------------------
    --[ File Permissions]--
    -----------------------


-- |Default permissions for a new file.
newFilePerms :: FileMode
newFilePerms
  =                  ownerWriteMode
    `unionFileModes` ownerReadMode
    `unionFileModes` groupWriteMode
    `unionFileModes` groupReadMode
    `unionFileModes` otherWriteMode
    `unionFileModes` otherReadMode


-- |Default permissions for a new directory.
newDirPerms :: FileMode
newDirPerms
  =                  ownerModes
    `unionFileModes` groupExecuteMode
    `unionFileModes` groupReadMode
    `unionFileModes` otherExecuteMode
    `unionFileModes` otherReadMode

