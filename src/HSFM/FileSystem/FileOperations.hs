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


-- |This module provides high-level IO related file operations like
-- copy, delete, move and so on. It only operates on `Path Abs` which
-- guarantees us well-typed paths which are absolute.
--
-- Some functions are just path-safe wrappers around
-- unix functions, others have stricter exception handling
-- and some implement functionality that doesn't have a unix
-- counterpart (like `copyDirRecursive`).
--
-- Some of these operations are due to their nature not _atomic_, which
-- means they may do multiple syscalls which form one context. Some
-- of them also have to examine the filetypes explicitly before the
-- syscalls, so a reasonable decision can be made. That means
-- the result is undefined if another process changes that context
-- while the non-atomic operation is still happening. However, where
-- possible, as few syscalls as possible are used and the underlying
-- exception handling is kept.
module HSFM.FileSystem.FileOperations where


import Control.Exception
  (
    bracket
  , bracketOnError
  , throw
  )
import Control.Monad
  (
    forM_
  , void
  , when
  )
import Data.ByteString
  (
    ByteString
  )
import Data.Foldable
  (
    for_
  )
import Data.Maybe
  (
    catMaybes
  )
import Data.Word
  (
    Word8
  )
import Foreign.C.Error
  (
    eINVAL
  , eNOSYS
  , eXDEV
  )
import Foreign.C.Types
  (
    CSize
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
import Prelude hiding (readFile)
import System.Posix.ByteString
  (
    exclusive
  )
import System.Posix.Directory.ByteString
  (
    createDirectory
  , removeDirectory
  )
import System.Posix.Directory.Traversals
  (
    getDirectoryContents'
  )
import System.Posix.Files.ByteString
  (
    createSymbolicLink
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
  , readSymbolicLink
  , removeLink
  , rename
  , unionFileModes
  )
import qualified System.Posix.Files.ByteString as PF
import qualified "unix" System.Posix.IO.ByteString as SPI
import qualified "unix-bytestring" System.Posix.IO.ByteString as SPB
import qualified System.Posix.Directory.Traversals as SPDT
import qualified System.Posix.Directory.Foreign as SPDF
import System.Posix.IO.Sendfile.ByteString
  (
    sendfileFd
  , FileRange(EntireFile)
  )
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
-- TODO: say which low-level syscalls are involved


-- |Data type describing an actual file operation that can be
-- carried out via `runFileOp`. Useful to build up a list of operations
-- or delay operations.
data FileOperation = FCopy    Copy
                   | FMove    Move
                   | FDelete  [Path Abs]
                   | FOpen    (Path Abs)
                   | FExecute (Path Abs) [ByteString]
                   | None


-- |Data type describing partial or complete file copy operation.
-- CC stands for a complete operation and can be used for `runFileOp`.
data Copy = PartialCopy [Path Abs]
          | Copy        [Path Abs] (Path Abs)


-- |Data type describing partial or complete file move operation.
-- MC stands for a complete operation and can be used for `runFileOp`.
data Move = PartialMove [Path Abs]
          | Move        [Path Abs] (Path Abs)


data FileType = Directory
              | RegularFile
              | SymbolicLink
              | BlockDevice
              | CharacterDevice
              | NamedPipe
              | Socket
  deriving (Eq, Show)



-- |Run a given FileOperation. If the FileOperation is partial, it will
-- be returned. Returns `Nothing` on success.
--
-- Since file operations can be delayed, this is `Path Abs` based, not
-- `File` based. This makes sure we don't have stale
-- file information.
runFileOp :: FileOperation -> IO (Maybe FileOperation)
runFileOp fo' =
  case fo' of
    (FCopy (Copy froms to)) -> do
      forM_ froms $ \x -> do
        toname <- P.basename x
        easyCopy x (to P.</> toname)
      return Nothing
    (FCopy fo) -> return $ Just $ FCopy fo
    (FMove (Move froms to)) -> do
      forM_ froms $ \x -> do
        toname <- P.basename x
        moveFile x (to P.</> toname)
      return Nothing
    (FMove fo) -> return $ Just $ FMove fo
    (FDelete fps) ->
      mapM_ easyDelete fps >> return Nothing
    (FOpen fp) -> openFile fp >> return Nothing
    (FExecute fp args) -> executeFile fp args >> return Nothing
    _ -> return Nothing



    --------------------
    --[ File Copying ]--
    --------------------



-- |Copies a directory to the given destination. Excludes symlinks.
--
-- Safety/reliability concerns:
--
--    * not atomic
--    * examines filetypes explicitly
--    * an explicit check `throwDestinationInSource` is carried out for the
--      top directory for basic sanity, because otherwise we might end up
--      with an infinite copy loop... however, this operation is not
--      carried out recursively (because it's slow)
--
-- Throws:
--
--    - `NoSuchThing` if source directory does not exist
--    - `PermissionDenied` if output directory is not writable
--    - `PermissionDenied` if source directory can't be opened
--    - `InvalidArgument` if source directory is wrong type (symlink)
--    - `InvalidArgument` if source directory is wrong type (regular file)
--    - `AlreadyExists` if source and destination are the same directory
--    - `AlreadyExists` if destination already exists
--    - `DestinationInSource` if destination is contained in source
copyDirRecursive :: Path Abs  -- ^ source dir
                 -> Path Abs  -- ^ full destination
                 -> IO ()
copyDirRecursive fromp destdirp
  = do
    -- for performance, sanity checks are only done for the top dir
    throwDestinationInSource fromp destdirp
    go fromp destdirp
  where
    go :: Path Abs -> Path Abs -> IO ()
    go fromp' destdirp' = do
      -- order is important here, so we don't get empty directories
      -- on failure
      contents <- getDirsFiles fromp'

      fmode' <- PF.fileMode <$> PF.getSymbolicLinkStatus (P.fromAbs fromp')
      createDirectory (P.fromAbs destdirp') fmode'

      for_ contents $ \f -> do
        ftype <- getFileType f
        newdest <- (destdirp' P.</>) <$> P.basename f
        case ftype of
          SymbolicLink -> recreateSymlink f newdest
          Directory    -> go f newdest
          RegularFile  -> copyFile f newdest
          _            -> ioError $ userError $ "No idea what to do with the" ++
                                                "given filetype: " ++ show ftype


-- |Recreate a symlink.
--
-- Throws:
--
--    - `InvalidArgument` if symlink file is wrong type (file)
--    - `InvalidArgument` if symlink file is wrong type (directory)
--    - `PermissionDenied` if output directory cannot be written to
--    - `PermissionDenied` if source directory cannot be opened
--    - `AlreadyExists` if destination file already exists
--    - `AlreadyExists` if destination and source are the same file
--
-- Note: calls `symlink`
recreateSymlink :: Path Abs  -- ^ the old symlink file
                -> Path Abs  -- ^ destination file
                -> IO ()
recreateSymlink symsource newsym
  = do
    sympoint <- readSymbolicLink (P.fromAbs symsource)
    createSymbolicLink sympoint (P.fromAbs newsym)


-- |Copies the given regular file to the given dir with the given filename.
-- Excludes symlinks.
--
-- Throws:
--
--    - `NoSuchThing` if source file does not exist
--    - `PermissionDenied` if output directory is not writable
--    - `PermissionDenied` if source directory can't be opened
--    - `InvalidArgument` if source file is wrong type (symlink)
--    - `InvalidArgument` if source file is wrong type (directory)
--    - `AlreadyExists` if source and destination are the same file
--    - `AlreadyExists` if destination already exists
--
-- Note: calls `sendfile`
copyFile :: Path Abs  -- ^ source file
         -> Path Abs  -- ^ destination file
         -> IO ()
copyFile from to
  =
    -- from sendfile(2) manpage:
    --   Applications  may  wish  to  fall back to read(2)/write(2) in the case
    --   where sendfile() fails with EINVAL or ENOSYS.
    P.withAbsPath to $ \to' -> P.withAbsPath from $ \from' ->
      catchErrno [eINVAL, eNOSYS]
                 (sendFileCopy from' to')
                 (void $ fallbackCopy from' to')
  where
    -- this is low-level stuff utilizing sendfile(2) for speed
    sendFileCopy source dest =
      bracket (SPDT.openFd source SPI.ReadOnly [SPDF.oNofollow] Nothing)
              SPI.closeFd
              $ \sfd -> do
                fileM <- System.Posix.Files.ByteString.fileMode
                         <$> getFdStatus sfd
                bracketeer (SPI.openFd dest SPI.WriteOnly (Just fileM)
                                    SPI.defaultFileFlags { exclusive = True })
                           SPI.closeFd
                           (\fd -> SPI.closeFd fd >> deleteFile to)
                           $ \dfd -> sendfileFd dfd sfd EntireFile
    -- low-level copy operation utilizing read(2)/write(2)
    -- in case `sendFileCopy` fails/is unsupported
    fallbackCopy source dest =
      bracket (SPDT.openFd source SPI.ReadOnly [SPDF.oNofollow] Nothing)
              SPI.closeFd
              $ \sfd -> do
                fileM <- System.Posix.Files.ByteString.fileMode
                         <$> getFdStatus sfd
                bracketeer (SPI.openFd dest SPI.WriteOnly (Just fileM)
                                    SPI.defaultFileFlags { exclusive = True })
                           SPI.closeFd
                           (\fd -> SPI.closeFd fd >> deleteFile to)
                            $ \dfd -> allocaBytes (fromIntegral bufSize) $ \buf ->
                                        write' sfd dfd buf 0
      where
        bufSize :: CSize
        bufSize = 8192
        write' :: Fd -> Fd -> Ptr Word8 -> Int -> IO Int
        write' sfd dfd buf totalsize = do
            size <- SPB.fdReadBuf sfd buf bufSize
            if size == 0
              then return $ fromIntegral totalsize
              else do rsize <- SPB.fdWriteBuf dfd buf size
                      when (rsize /= size) (throw . CopyFailed $ "wrong size!")
                      write' sfd dfd buf (totalsize + fromIntegral size)


-- |Copies anything. In case of a symlink,
-- it is just recreated, even if it points to a directory.
--
-- Safety/reliability concerns:
--
--    * examines filetypes explicitly
--    * calls `copyDirRecursive` for directories
easyCopy :: Path Abs
         -> Path Abs
         -> IO ()
easyCopy from to = do
  ftype <- getFileType from
  case ftype of
       SymbolicLink -> recreateSymlink from to
       RegularFile  -> copyFile from to
       Directory    -> copyDirRecursive from to
       _            -> ioError $ userError $ "No idea what to do with the" ++
                                             "given filetype: " ++ show ftype






    ---------------------
    --[ File Deletion ]--
    ---------------------


-- |Deletes the given file, does not follow symlinks. Raises `eISDIR`
-- if run on a directory.
deleteFile :: Path Abs -> IO ()
deleteFile p = P.withAbsPath p removeLink


-- |Deletes the given directory, which must be empty, never symlinks.
deleteDir :: Path Abs -> IO ()
deleteDir p = P.withAbsPath p removeDirectory


-- |Deletes the given directory recursively.
--
-- Safety/reliability concerns:
--
--    * not atomic
--    * examines filetypes explicitly
deleteDirRecursive :: Path Abs -> IO ()
deleteDirRecursive p = do
  files <- getDirsFiles p
  for_ files $ \file -> do
    ftype <- getFileType file
    case ftype of
      SymbolicLink -> deleteFile file
      Directory    -> deleteDirRecursive file
      RegularFile  -> deleteFile file
      _            -> ioError $ userError $ "No idea what to do with the" ++
                                            "given filetype: " ++ show ftype
  removeDirectory . P.toFilePath $ p


-- |Deletes a file, directory or symlink, whatever it may be.
-- In case of directory, performs recursive deletion. In case of
-- a symlink, the symlink file is deleted.
--
-- Safety/reliability concerns:
--
--    * examines filetypes explicitly
--    * calls `deleteDirRecursive` for directories
easyDelete :: Path Abs -> IO ()
easyDelete p = do
  ftype <- getFileType p
  case ftype of
    SymbolicLink -> deleteFile p
    Directory    -> deleteDirRecursive p
    RegularFile  -> deleteFile p
    _            -> ioError $ userError $ "No idea what to do with the" ++
                                          "given filetype: " ++ show ftype




    --------------------
    --[ File Opening ]--
    --------------------


-- |Opens a file appropriately by invoking xdg-open. The file type
-- is not checked.
openFile :: Path Abs
         -> IO ProcessID
openFile p =
  P.withAbsPath p $ \fp ->
    SPP.forkProcess $ SPP.executeFile "xdg-open" True [fp] Nothing


-- |Executes a program with the given arguments.
executeFile :: Path Abs        -- ^ program
            -> [ByteString]    -- ^ arguments
            -> IO ProcessID
executeFile fp args
  = P.withAbsPath fp $ \fpb ->
      SPP.forkProcess
      $ SPP.executeFile fpb True args Nothing




    ---------------------
    --[ File Creation ]--
    ---------------------


-- |Create an empty regular file at the given directory with the given filename.
--
-- Throws:
--
--    - `PermissionDenied` if output directory cannot be written to
--    - `AlreadyExists` if destination file already exists
createRegularFile :: Path Abs -> IO ()
createRegularFile dest =
  bracket (SPI.openFd (P.fromAbs dest) SPI.WriteOnly (Just newFilePerms)
                      (SPI.defaultFileFlags { exclusive = True }))
          SPI.closeFd
          (\_ -> return ())


-- |Create an empty directory at the given directory with the given filename.
--
-- Throws:
--
--    - `PermissionDenied` if output directory cannot be written to
--    - `AlreadyExists` if destination directory already exists
createDir :: Path Abs -> IO ()
createDir dest = createDirectory (P.fromAbs dest) newDirPerms




    ----------------------------
    --[ File Renaming/Moving ]--
    ----------------------------


-- |Rename a given file with the provided filename. Destination and source
-- must be on the same device, otherwise `eXDEV` will be raised.
--
-- Safety/reliability concerns:
--
--    * has a separate set of exception handling, apart from the syscall
--
-- Throws:
--
--     - `NoSuchThing` if source file does not exist
--     - `PermissionDenied` if output directory cannot be written to
--     - `PermissionDenied` if source driectory cannot be opened
--     - `UnsupportedOperation` if source and destination are on different devices
--     - `FileDoesExist` if destination file already exists
--     - `DirDoesExist` if destination directory already exists
--     - `SameFile` if destination and source are the same file
--
-- Note: calls `rename` (but does not allow to rename over existing files)
renameFile :: Path Abs -> Path Abs -> IO ()
renameFile fromf tof = do
  throwSameFile fromf tof
  throwFileDoesExist tof
  throwDirDoesExist tof
  rename (P.fromAbs fromf) (P.fromAbs tof)


-- |Move a file. This also works across devices by copy-delete fallback.
-- And also works on directories.
--
-- Safety/reliability concerns:
--
--    * copy-delete fallback is inherently non-atomic
--
-- Throws:
--
--     - `NoSuchThing` if source file does not exist
--     - `PermissionDenied` if output directory cannot be written to
--     - `PermissionDenied` if source driectory cannot be opened
--     - `FileDoesExist` if destination file already exists
--     - `DirDoesExist` if destination directory already exists
--     - `SameFile` if destination and source are the same file
--
-- Note: calls `rename` (but does not allow to rename over existing files)
moveFile :: Path Abs  -- ^ file to move
         -> Path Abs  -- ^ destination
         -> IO ()
moveFile from to =
  catchErrno [eXDEV] (renameFile from to) $ do
    easyCopy from to
    easyDelete from





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



    -------------------------
    --[ Directory reading ]--
    -------------------------


-- |Gets all filenames of the given directory. This excludes "." and "..".
getDirsFiles :: Path Abs        -- ^ dir to read
             -> IO [Path Abs]
getDirsFiles p =
  P.withAbsPath p $ \fp ->
    bracketOnError (SPDT.openFd fp SPI.ReadOnly [SPDF.oNofollow] Nothing)
            SPI.closeFd
            $ \fd ->
              return
                . catMaybes
                .   fmap (\x -> (P.</>) p <$> (parseMaybe . snd $ x))
                =<< getDirectoryContents' fd
  where
    parseMaybe :: ByteString -> Maybe (Path Fn)
    parseMaybe = P.parseFn




    ---------------------------
    --[ FileType operations ]--
    ---------------------------


-- |Get the file type of the file located at the given path. Does
-- not follow symbolic links.
--
-- Throws:
--
--    - `NoSuchThing` if the file does not exist
--    - `PermissionDenied` if any part of the path is not accessible
getFileType :: Path Abs -> IO FileType
getFileType p = do
  fs <- PF.getSymbolicLinkStatus (P.fromAbs p)
  decide fs
  where
    decide fs
      | PF.isDirectory fs       = return Directory
      | PF.isRegularFile fs     = return RegularFile
      | PF.isSymbolicLink fs    = return SymbolicLink
      | PF.isBlockDevice fs     = return BlockDevice
      | PF.isCharacterDevice fs = return CharacterDevice
      | PF.isNamedPipe fs       = return NamedPipe
      | PF.isSocket fs          = return Socket
      | otherwise               = ioError $ userError "No filetype?!"

