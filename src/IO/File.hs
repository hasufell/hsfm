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
  , when
  )
import Data.DirTree
import Data.DirTree.Zipper
import Data.Foldable
  (
    for_
  )
import IO.Error
import System.Directory
  (
    canonicalizePath
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , executable
  , removeDirectory
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
data FileOperation = FCopy DTInfoZipper DTInfoZipper
                       | FMove FilePath FilePath
                       | FDelete DTInfoZipper
                       | FOpen DTInfoZipper
                       | FExecute DTInfoZipper [String]
                       | None


runFileOp :: FileOperation -> IO ()
runFileOp (FCopy from@(File {}, _) to) = copyFileToDir from to
runFileOp (FCopy from@(Dir {}, _) to) = copyDir from to
runFileOp (FDelete fp)       = easyDelete fp
runFileOp (FOpen fp)         = void $ openFile fp
runFileOp (FExecute fp args) = void $ executeFile fp args
runFileOp _                  = return ()


-- TODO: copy modes
copyDir :: DTInfoZipper  -- ^ source dir
        -> DTInfoZipper  -- ^ destination dir
        -> IO ()
copyDir from@(Dir fn _ _, _) to@(Dir {}, _) = do
  let fromp = getFullPath from
      top   = getFullPath to
      destdir = getFullPath to </> fn

  dirSanityThrow fromp
  dirSanityThrow top
  throwDestinationInSource fromp top

  createDirectoryIfMissing False destdir

  for_ (goAllDown from) $ \f -> do
    newDest <- zipLazy mkDirInfo mkFileInfo destdir
    case f of
      -- recreate symlink
      sz@(Dir { name = n, dir = (DirInfo { sym = True }) }, _)  -> do
        let sympoint = getFullPath newDest </> n
        -- delete old file/dir to be able to create symlink
        for_ (goDown n newDest) $ \odtz ->
          easyDelete odtz
        symname <- readSymbolicLink (getFullPath sz)
        createSymbolicLink symname sympoint
      sz@(Dir {}, _)  ->
        copyDir sz newDest
      sz@(File {}, _) ->
        copyFileToDir sz newDest
copyDir from@(File _ _, _) _ = throw $ NotADir (getFullPath from)
copyDir _ to@(File _ _, _)   = throw $ NotADir (getFullPath to)



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

