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
import IO.Error
import System.Directory
  (
    canonicalizePath
  , doesDirectoryExist
  , doesFileExist
  , executable
  , getPermissions
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
import System.Process
  (
    spawnProcess
  , ProcessHandle
  )

import qualified System.Directory as SD


-- TODO: modify the DTZipper directly after file operations!?


-- |Data type describing an actual file operation that can be
-- carried out via `doFile`. Useful to build up a list of operations
-- or delay operations.
data FileOperation a b = FCopy (DTZipper a b) (DTZipper a b)
                       | FMove FilePath FilePath
                       | FDelete (DTZipper a b)
                       | FOpen (DTZipper a b)
                       | FExecute (DTZipper a b) [String]
                       | None


runFileOp :: FileOperation a b -> IO ()
runFileOp (FCopy from to)    = copyFileToDir from to
runFileOp (FDelete fp)       = easyDelete fp
runFileOp (FOpen fp)         = void $ openFile fp
runFileOp (FExecute fp args) = void $ executeFile fp args
runFileOp _                  = return ()


-- |Copies the given file.
--
-- This will throw an exception if any of the filepaths are not absolute
-- and an exception if the source file does not exist.
--
-- If the destination file already exists, it will be replaced.
-- TODO: don't permit copying file A to file A
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


-- |Deletes the given file.
--
-- This will throw an exception if the filepath is not absolute
-- or the file does not exist.
--
-- It also throws exceptions from `removeFile`.
-- TODO: threaded, shouldn't block the GUI
deleteFile :: DTZipper a b -> IO ()
deleteFile dtz@(File {}, _) = do
  let fp = getFullPath dtz
  fileSanityThrow fp
  removeFile fp
deleteFile dtz = throw $ NotAFile (getFullPath dtz)


-- |Deletes the given directory.
--
-- This will throw an exception if the filepath is not absolute
-- or the directory does not exist.
--
-- It also throws exceptions from `removeDirectory`.
-- TODO: threaded, shouldn't block the GUI
deleteDir :: DTZipper a b -> IO ()
deleteDir dtz@(Dir {}, _) = do
  let fp = getFullPath dtz
  dirSanityThrow fp
  removeDirectory fp
deleteDir dtz = throw $ NotADir (getFullPath dtz)


-- |Deletes a file or directory, whatever it may be.
easyDelete :: DTZipper a b -> IO ()
easyDelete dtz@(File {}, _) = deleteFile dtz
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
executeFile :: DTZipper a b      -- ^ program
            -> [String]          -- ^ arguments
            -> IO ProcessHandle
executeFile dtz@(File {}, _) args = do
  let fp = getFullPath dtz
  fileSanityThrow fp
  p <- getPermissions fp
  unless (executable p) (throw $ FileNotExecutable fp)
  spawnProcess fp args
executeFile dtz  _ = throw $ NotAFile (getFullPath dtz)

