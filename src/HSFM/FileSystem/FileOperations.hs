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
import HSFM.FileSystem.FileType
import HSFM.Utils.IO
import Prelude hiding (readFile)
import System.Posix.Directory.ByteString
  (
    createDirectory
  , removeDirectory
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
-- `File` based. This makes sure we don't have stale
-- file information.
runFileOp :: FileOperation -> IO (Maybe FileOperation)
runFileOp fo' =
  case fo' of
    (FCopy (CC froms to cm)) -> do
      froms' <- mapM toAfile froms
      to'    <- toAfile to
      when (anyFailed froms')
           (throw . CopyFailed $ "File in copy buffer does not exist anymore!")
      mapM_ (\x -> easyCopy cm x to') froms'
        >> return Nothing
    (FCopy fo) -> return $ Just $ FCopy fo
    (FMove (MC froms to cm)) -> do
      froms' <- mapM toAfile froms
      to'   <- toAfile to
      when (anyFailed froms')
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
        -> File a  -- ^ source dir
        -> File a  -- ^ destination dir
        -> Path Fn -- ^ destination dir name
        -> IO ()
copyDir       (Rename fn)
         from@Dir{}
           to@Dir{}
           _
  = copyDir Strict from to fn
-- this branch must never get `Rename` as CopyMode
copyDir cm from@Dir{ path = fromp }
             to@Dir{ path = top }
             fn
  = do
    let destdirp = top P.</> fn
    -- for performance, sanity checks are only done for the top dir
    throwDestinationInSource fromp destdirp
    throwSameFile fromp destdirp
    throwCantOpenDirectory fromp
    throwCantOpenDirectory top
    go cm from to fn
  where
    go :: CopyMode -> File a -> File a -> Path Fn -> IO ()
    go cm' Dir{ path = fromp' }
           Dir{ path = top' }
           fn' = do
      fmode' <- PF.fileMode <$> PF.getSymbolicLinkStatus
                                   (P.fromAbs fromp')
      createDestdir (top' P.</> fn') fmode'
      destdir <- readFile (\_ -> return undefined)
                   (top' P.</> fn')
      contents <- readDirectoryContents
                    (\_ -> return undefined) fromp'

      for_ contents $ \f ->
        case f of
          SymLink{ path = fp' }  -> recreateSymlink cm' f destdir
                                      =<< (P.basename fp')
          Dir{ path = fp' }      -> go cm' f destdir
                                      =<< (P.basename fp')
          RegFile{ path = fp' }  -> unsafeCopyFile Replace f destdir
                                      =<< (P.basename fp')
          _                      -> return ()
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
                       readFile
                         (\_ -> return undefined) destdir)
              createDirectory destdir' fmode'
            _ -> throw $ InvalidOperation "Internal error, wrong CopyMode!"
    go _ _ _ _ = throw $ InvalidOperation "wrong input type"
copyDir _ _ _ _ = throw $ InvalidOperation "wrong input type"


-- |Recreate a symlink.
recreateSymlink :: CopyMode
                -> File a  -- ^ the old symlink file
                -> File a  -- ^ destination dir of the
                           --   new symlink file
                -> Path Fn -- ^ destination file name
                -> IO ()
recreateSymlink (Rename pn) symf@SymLink{} symdest@Dir{} _
  = recreateSymlink Strict symf symdest pn
recreateSymlink cm SymLink{ path = sfp } Dir{ path = sdp } fn
  = do
    throwCantOpenDirectory sdp
    sympoint <- readSymbolicLink (P.fromAbs sfp)
    let symname = sdp P.</> fn
    case cm of
      Merge   -> delOld symname
      Replace -> delOld symname
      _       -> return ()
    createSymbolicLink sympoint (P.fromAbs symname)
  where
    delOld symname = do
      f <- readFile (\_ -> return undefined) symname
      unless (failed f)
             (easyDelete f)
recreateSymlink _ _ _ _ = throw $ InvalidOperation "wrong input type"


-- |Copies the given regular file to the given dir with the given filename.
-- Excludes symlinks.
copyFile :: CopyMode
         -> File a  -- ^ source file
         -> File a  -- ^ destination dir
         -> Path Fn -- ^ destination file name
         -> IO ()
copyFile (Rename pn) from@RegFile{} to@Dir{} _
  = copyFile Strict from to pn
copyFile cm from@RegFile{ path = fromp }
             tod@Dir{ path = todp } fn
  = do
    throwCantOpenDirectory todp
    throwCantOpenDirectory . P.dirname $ fromp
    throwSameFile fromp (todp P.</> fn)
    unsafeCopyFile cm from tod fn
copyFile _ _ _ _ = throw $ InvalidOperation "wrong input type"


-- |Unsafe version of `copyFile` without initial sanity checks. This
-- holds the actual copy logic though and is called by `copyFile` in the end.
-- It's also used for cases where we don't need/want sanity checks
-- and need the extra bit of performance.
unsafeCopyFile :: CopyMode
               -> File a  -- ^ source file
               -> File a  -- ^ destination dir
               -> Path Fn -- ^ destination file name
               -> IO ()
unsafeCopyFile (Rename pn) from@RegFile{} to@Dir{} _
  = copyFile Strict from to pn
unsafeCopyFile cm RegFile{ path = fromp }
                  Dir{ path = todp } fn
  = do
    let to = todp P.</> fn
    case cm of
      Strict -> throwFileDoesExist to
      _      -> return ()

    -- from sendfile(2) manpage:
    --   Applications  may  wish  to  fall back to read(2)/write(2) in the case
    --   where sendfile() fails with EINVAL or ENOSYS.
    P.withAbsPath to $ \to' -> P.withAbsPath fromp $ \from' ->
      catchErrno [eINVAL, eNOSYS]
                 (sendFileCopy from' to')
                 (void $ fallbackCopy from' to')
  where
    -- this is low-level stuff utilizing sendfile(2) for speed
    sendFileCopy source dest =
      -- NOTE: we are not blocking IO here, O_NONBLOCK is false
      -- for `defaultFileFlags`
      bracket (SPI.openFd source SPI.ReadOnly Nothing SPI.defaultFileFlags)
              SPI.closeFd
              $ \sfd -> do
                fileM <- System.Posix.Files.ByteString.fileMode
                         <$> getFdStatus sfd
                bracket (SPI.openFd dest SPI.WriteOnly (Just fileM)
                                    SPI.defaultFileFlags)
                        SPI.closeFd
                        $ \dfd -> sendfileFd dfd sfd EntireFile
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
unsafeCopyFile _ _ _ _ = throw $ InvalidOperation "wrong input type"


-- |Copies a regular file, directory or symlink. In case of a symlink,
-- it is just recreated, even if it points to a directory.
easyCopy :: CopyMode
         -> File a
         -> File a
         -> IO ()
easyCopy cm from@SymLink{}
              to@Dir{}
  = recreateSymlink cm from to =<< (P.basename . path $ from)
easyCopy cm from@RegFile{}
              to@Dir{}
  = copyFile cm from to =<< (P.basename . path $ from)
easyCopy cm from@Dir{}
              to@Dir{}
  = copyDir cm from to =<< (P.basename . path $ from)
easyCopy _ _ _ = throw $ InvalidOperation "wrong input type"





    ---------------------
    --[ File Deletion ]--
    ---------------------


-- |Deletes a symlink, which can either point to a file or directory.
deleteSymlink :: File a -> IO ()
deleteSymlink SymLink{ path = fp }
  = P.withAbsPath fp removeLink
deleteSymlink _ = throw $ InvalidOperation "wrong input type"


-- |Deletes the given regular file, never symlinks.
deleteFile :: File a -> IO ()
deleteFile RegFile{ path = fp }
  = P.withAbsPath fp removeLink
deleteFile _ = throw $ InvalidOperation "wrong input type"


-- |Deletes the given directory, never symlinks.
deleteDir :: File a -> IO ()
deleteDir Dir{ path = fp }
  = P.withAbsPath fp removeDirectory
deleteDir _ = throw $ InvalidOperation "wrong input type"


-- |Deletes the given directory recursively.
deleteDirRecursive :: File a -> IO ()
deleteDirRecursive f'@Dir{ path = fp' } = do
  throwCantOpenDirectory fp'
  go f'
  where
    go :: File a -> IO ()
    go Dir{ path = fp } = do
      files <- readDirectoryContents
                 (\_ -> return undefined) fp
      for_ files $ \file ->
        case file of
          SymLink{} -> deleteSymlink file
          Dir{}     -> go file
          RegFile{ path = rfp }
                    -> P.withAbsPath rfp removeLink
          _         -> throw $ FileDoesExist
                               (P.toFilePath . path $ file)
      removeDirectory . P.toFilePath $ fp
    go _ = throw $ InvalidOperation "wrong input type"
deleteDirRecursive _ = throw $ InvalidOperation "wrong input type"


-- |Deletes a file, directory or symlink, whatever it may be.
-- In case of directory, performs recursive deletion. In case of
-- a symlink, the symlink file is deleted.
easyDelete :: File a -> IO ()
easyDelete f@SymLink{} = deleteSymlink f
easyDelete f@RegFile{}
  = deleteFile f
easyDelete f@Dir{}
  = deleteDirRecursive f
easyDelete _ = throw $ InvalidOperation "wrong input type"




    --------------------
    --[ File Opening ]--
    --------------------


-- |Opens a file appropriately by invoking xdg-open. The file type
-- is not checked.
openFile :: File a
         -> IO ProcessID
openFile f =
  P.withAbsPath (path f) $ \fp ->
    SPP.forkProcess $ SPP.executeFile "xdg-open" True [fp] Nothing


-- |Executes a program with the given arguments.
executeFile :: File a  -- ^ program
            -> [ByteString]    -- ^ arguments
            -> IO ProcessID
executeFile RegFile{ path = fp } args
  = P.withAbsPath fp $ \fpb ->
      SPP.forkProcess
      $ SPP.executeFile fpb True args Nothing
executeFile SymLink{ path = fp, sdest = RegFile{} } args
  = P.withAbsPath fp $ \fpb ->
      SPP.forkProcess
      $ SPP.executeFile fpb True args Nothing
executeFile _ _ = throw $ InvalidOperation "wrong input type"




    ---------------------
    --[ File Creation ]--
    ---------------------


-- |Create an empty regular file at the given directory with the given filename.
createFile :: File FileInfo -> Path Fn -> IO ()
createFile (DirOrSym td) fn = do
  let fullp = path td P.</> fn
  throwFileDoesExist fullp
  fd <- SPI.createFile (P.fromAbs fullp) newFilePerms
  SPI.closeFd fd
createFile _ _ = throw $ InvalidOperation "wrong input type"


-- |Create an empty directory at the given directory with the given filename.
createDir :: File FileInfo -> Path Fn -> IO ()
createDir (DirOrSym td) fn = do
  let fullp = path td P.</> fn
  throwDirDoesExist fullp
  createDirectory (P.fromAbs fullp) newFilePerms
createDir _ _ = throw $ InvalidOperation "wrong input type"




    ----------------------------
    --[ File Renaming/Moving ]--
    ----------------------------


-- |Rename a given file with the provided filename.
renameFile :: File a -> Path Fn -> IO ()
renameFile af fn = do
  let fromf = path af
      tof   = (P.dirname . path $ af) P.</> fn
  throwFileDoesExist tof
  throwSameFile fromf tof
  rename (P.fromAbs fromf) (P.fromAbs tof)


-- |Move a given file to the given target directory.
moveFile :: CopyMode
         -> File a  -- ^ file to move
         -> File a  -- ^ base target directory
         -> Path Fn -- ^ target file name
         -> IO ()
moveFile (Rename pn) from to@Dir{} _ =
  moveFile Strict from to pn
moveFile cm from to@Dir{} fn = do
  let from'  = path from
      froms' = P.fromAbs from'
      to'    = path to P.</> fn
      tos'   = P.fromAbs to'
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
      to' <- readFile (\_ -> return undefined) fp
      unless (failed to') (easyDelete to')
moveFile _ _ _ _ = throw $ InvalidOperation "wrong input type"


-- |Like `moveFile` except it uses the filename of the source as target.
easyMove :: CopyMode
         -> File a -- ^ file to move
         -> File a -- ^ base target directory
         -> IO ()
easyMove cm from to = moveFile cm from to =<< (P.basename . path $ from)



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

