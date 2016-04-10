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
{-# OPTIONS_HADDOCK ignore-exports #-}

-- |This module provides data types for representing directories/files
-- and related operations on it, mostly internal stuff.
--
-- It doesn't allow to represent the whole filesystem, since that's only
-- possible through IO laziness, which introduces too much internal state.
module HSFM.FileSystem.FileType where



import Control.Exception
  (
    bracket
  )
import Data.ByteString(ByteString)
import qualified Data.ByteString as B
import Data.Default
import Data.Time.Clock.POSIX
  (
    POSIXTime
  , posixSecondsToUTCTime
  )
import Data.Time()
import Foreign.C.Error
  (
    eACCES
  )
import HPath
  (
    Abs
  , Path
  , Fn
  , pattern Path
  )
import qualified HPath as P
import HSFM.FileSystem.Errors
import HSFM.Utils.MyPrelude
import Prelude hiding(readFile)
import System.IO.Error
  (
    ioeGetErrorType
  , isDoesNotExistErrorType
  )
import qualified System.Posix.Directory.ByteString as PFD
import qualified System.Posix.Files.ByteString as PF
import System.Posix.Types
  (
    DeviceID
  , EpochTime
  , FileID
  , FileMode
  , FileOffset
  , GroupID
  , LinkCount
  , UserID
  )






    ----------------------------
    --[ BASE TYPES ]--
    ----------------------------


-- |Represents a file. The `anchor` field is the path
-- to that file without the filename.
data AnchoredFile a =
  (:/) { anchor :: Path Abs, file :: File a }
  deriving (Eq, Show)


-- |The String in the name field is always a file name, never a full path.
-- The free type variable is used in the File/Dir constructor and can hold
-- Handles, Strings representing a file's contents or anything else you can
-- think of. We catch any IO errors in the Failed constructor. an Exception
-- can be converted to a String with 'show'.
data File a =
    Failed {
    name :: Path Fn
  , err  :: IOError
  }
  | Dir {
    name :: Path Fn
  , fvar :: a
  }
  | RegFile {
    name :: Path Fn
  , fvar :: a
  }
  | SymLink {
    name    :: Path Fn
  , fvar    :: a
  , sdest   :: AnchoredFile a  -- ^ symlink madness,
                               --   we need to know where it points to
  , rawdest :: ByteString
  }
  | BlockDev {
    name :: Path Fn
  , fvar :: a
  }
  | CharDev {
    name :: Path Fn
  , fvar :: a
  }
  | NamedPipe {
    name :: Path Fn
  , fvar :: a
  }
  | Socket {
    name :: Path Fn
  , fvar :: a
  } deriving (Show, Eq)


-- |Low-level file information.
data FileInfo = FileInfo {
    deviceID :: DeviceID
  , fileID :: FileID
  , fileMode :: FileMode
  , linkCount :: LinkCount
  , fileOwner :: UserID
  , fileGroup :: GroupID
  , specialDeviceID :: DeviceID
  , fileSize :: FileOffset
  , accessTime :: EpochTime
  , modificationTime :: EpochTime
  , statusChangeTime :: EpochTime
  , accessTimeHiRes :: POSIXTime
  , modificationTimeHiRes :: POSIXTime
  , statusChangeTimeHiRes :: POSIXTime
} deriving (Show, Eq, Ord)




    ------------------------------------
    --[ ViewPatterns/PatternSynonyms ]--
    ------------------------------------


-- |Converts a viewpattern like function written for `File` to one
-- for `AnchoredFile`.
convertViewP :: (File FileInfo -> (Bool, File FileInfo))
             -> AnchoredFile FileInfo
             -> (Bool, AnchoredFile FileInfo)
convertViewP f (bp :/ constr) =
  let (b, file) = f constr
  in (b, bp :/ file)



---- Filetypes ----


safileLike :: AnchoredFile FileInfo -> (Bool, AnchoredFile FileInfo)
safileLike f = convertViewP sfileLike f


sfileLike :: File FileInfo -> (Bool, File FileInfo)
sfileLike f@RegFile{}   = (True, f)
sfileLike f@BlockDev{}  = (True, f)
sfileLike f@CharDev{}   = (True, f)
sfileLike f@NamedPipe{} = (True, f)
sfileLike f@Socket{}    = (True, f)
sfileLike f             = fileLikeSym f


afileLike :: AnchoredFile FileInfo -> (Bool, AnchoredFile FileInfo)
afileLike f = convertViewP fileLike f


fileLike :: File FileInfo -> (Bool, File FileInfo)
fileLike f@RegFile {}  = (True, f)
fileLike f@BlockDev{}  = (True, f)
fileLike f@CharDev{}   = (True, f)
fileLike f@NamedPipe{} = (True, f)
fileLike f@Socket{}    = (True, f)
fileLike f             = (False, f)


sadir :: AnchoredFile FileInfo -> (Bool, AnchoredFile FileInfo)
sadir f = convertViewP sdir f


sdir :: File FileInfo -> (Bool, File FileInfo)
sdir f@SymLink{ sdest = (_ :/ s@SymLink{} )}
  -- we have to follow a chain of symlinks here, but
  -- return only the very first level
  -- TODO: this is probably obsolete now
  = case sdir s of
      (True, _) -> (True, f)
      _         -> (False, f)
sdir f@SymLink{ sdest = (_ :/ Dir {} )}
  = (True, f)
sdir f@Dir{} = (True, f)
sdir f       = (False, f)


-- |Matches on any non-directory kind of files, excluding symlinks.
pattern AFileLike f <- (afileLike -> (True, f))
-- |Like `AFileLike`, except on File.
pattern FileLike  f <- (fileLike  -> (True, f))

-- |Matches a list of directories or symlinks pointing to directories.
pattern DirList fs <- (\fs -> (and . fmap (fst . sadir) $ fs, fs)
                                -> (True, fs))

-- |Matches a list of any non-directory kind of files or symlinks
-- pointing to such.
pattern FileLikeList fs <- (\fs -> (and
                                     . fmap (fst . safileLike)
                                     $ fs, fs) -> (True, fs))


---- Filenames ----

invalidFileName :: Path Fn -> (Bool, Path Fn)
invalidFileName p@(Path "")   = (True, p)
invalidFileName p@(Path ".")  = (True, p)
invalidFileName p@(Path "..") = (True, p)
invalidFileName p             = (B.elem P.pathSeparator (P.fromRel p), p)


-- |Matches on invalid filesnames, such as ".", ".." and anything
-- that contains a path separator.
pattern InvFN   <- (invalidFileName -> (True,_))
-- |Opposite of `InvFN`.
pattern ValFN f <- (invalidFileName -> (False, f))

-- |Like `InvFN`, but for AnchoredFile.
pattern AFileInvFN  <- (fst . invalidFileName . name . file -> True)
-- |Like `InvFN`, but for File.
pattern FileInvFN   <- (fst . invalidFileName . name        -> True)


---- Symlinks ----

abrokenSymlink :: AnchoredFile FileInfo -> (Bool, AnchoredFile FileInfo)
abrokenSymlink f = convertViewP brokenSymlink f


brokenSymlink :: File FileInfo -> (Bool, File FileInfo)
brokenSymlink f = (isBrokenSymlink f, f)


afileLikeSym :: AnchoredFile FileInfo -> (Bool, AnchoredFile FileInfo)
afileLikeSym f = convertViewP fileLikeSym f


fileLikeSym :: File FileInfo -> (Bool, File FileInfo)
fileLikeSym f@SymLink{ sdest = (_ :/ s@SymLink{} )}
  = case fileLikeSym s of
      (True, _) -> (True, f)
      _         -> (False, f)
fileLikeSym f@SymLink{ sdest = (_ :/ RegFile {} )}   = (True, f)
fileLikeSym f@SymLink{ sdest = (_ :/ BlockDev {} )}  = (True, f)
fileLikeSym f@SymLink{ sdest = (_ :/ CharDev {} )}   = (True, f)
fileLikeSym f@SymLink{ sdest = (_ :/ NamedPipe {} )} = (True, f)
fileLikeSym f@SymLink{ sdest = (_ :/ Socket {} )}    = (True, f)
fileLikeSym f                                        = (False, f)


adirSym :: AnchoredFile FileInfo -> (Bool, AnchoredFile FileInfo)
adirSym f = convertViewP dirSym f


dirSym :: File FileInfo -> (Bool, File FileInfo)
dirSym f@SymLink{ sdest = (_ :/ s@SymLink{} )}
  = case dirSym s of
      (True, _) -> (True, f)
      _         -> (False, f)
dirSym f@SymLink{ sdest = (_ :/ Dir {} )} = (True, f)
dirSym f = (False, f)


-- |Matches on symlinks pointing to file-like files only.
pattern AFileLikeSym f <- (afileLikeSym -> (True, f))
-- |Like `AFileLikeSym`, except on File.
pattern FileLikeSym  f <- (fileLikeSym  -> (True, f))

-- |Matches on broken symbolic links.
pattern ABrokenSymlink f <- (abrokenSymlink -> (True, f))
-- |Like `ABrokenSymlink`, except on File.
pattern BrokenSymlink  f <- (brokenSymlink  -> (True, f))

-- |Matches on directories or symlinks pointing to directories.
-- If the symlink is pointing to a symlink pointing to a directory, then
-- it will return True, but also return the first element in the symlink-
-- chain, not the last.
pattern ADirOrSym f <- (sadir -> (True, f))
-- |Like `ADirOrSym`, except on File.
pattern DirOrSym  f <- (sdir  -> (True, f))

-- |Matches on symlinks pointing to directories only.
pattern ADirSym  f <- (adirSym -> (True, f))
-- |Like `ADirSym`, except on File.
pattern DirSym   f <- (dirSym  -> (True, f))

-- |Matches on any non-directory kind of files or symlinks pointing to
-- such.
-- If the symlink is pointing to a symlink pointing to such a file, then
-- it will return True, but also return the first element in the symlink-
-- chain, not the last.
pattern AFileLikeOrSym f <- (safileLike -> (True, f))
-- |Like `AFileLikeOrSym`, except on File.
pattern FileLikeOrSym  f <- (sfileLike  -> (True, f))





    -----------------
    --[ INSTANCES ]--
    -----------------


-- | First compare constructors: Failed < Dir < File...
-- Then compare `name`...
-- Then compare free variable parameter of `File` constructors
instance Ord (File FileInfo) where
    compare (RegFile n a) (RegFile n' a') =
        case compare n n' of
             EQ -> compare a a'
             el -> el
    compare (Dir n b) (Dir n' b') =
        case compare n n' of
             EQ -> compare b b'
             el -> el
     -- after comparing above we can hand off to shape ord function:
    compare d d' = comparingConstr d d'


-- |First compare anchor, then compare File.
instance Ord (AnchoredFile FileInfo) where
  compare (bp1 :/ a) (bp2 :/ b) =
    case compare bp1 bp2 of
      EQ -> compare a b
      el -> el





    ----------------------------
    --[ HIGH LEVEL FUNCTIONS ]--
    ----------------------------


-- |Reads a file or directory Path into an `AnchoredFile`, filling the free
-- variables via the given function.
-- The dirname of the given path will be canonicalized using `realpath`, so the
-- anchor of `AnchoredFile` is always canonicalized.
--
-- Exceptions: when `canonicalizePath` fails, throws IOError
readFile :: (Path Abs -> IO a)  -- ^ function that fills the free
                                --   a variable
         -> Path Abs            -- ^ Path to read
         -> IO (AnchoredFile a)
readFile ff p = do
  cdp <- P.canonicalizePath (P.dirname p)
  readFileUnsafe ff (cdp P.</> P.basename p)


-- |A variant of `readFile` which does not use `realpath` at all.
-- Suitable for cases where we know the paths are safe/correct
-- and need the extra bit of performance.
readFileUnsafe :: (Path Abs -> IO a)
               -> Path Abs
               -> IO (AnchoredFile a)
readFileUnsafe ff p = do
  let fn = P.basename p
      bd = P.dirname p
      p' = P.toFilePath p
  handleDT bd fn $ do
    fs   <- PF.getSymbolicLinkStatus p'
    fv   <- ff p
    file <- constructFile fs fv bd fn
    return (bd :/ file)
  where
    constructFile fs fv bd' fn'
      | PF.isSymbolicLink    fs = do
          -- symlink madness, we need to make sure we save the correct
          -- AnchoredFile
          let fp = bd' P.</> fn'
          x <- PF.readSymbolicLink (P.fromAbs fp)
          resolvedSyml <- handleDT bd' fn' $ do
            -- watch out, we call </> from 'filepath' here, but it is safe
            -- TODO: could it happen that too many '..' lead
            -- to something like '/' after normalization?
            let sfp = P.fromAbs bd' `P.combine` x
            rsfp <- P.realPath sfp
            readFile ff =<< P.parseAbs rsfp
          return $ SymLink fn' fv resolvedSyml x
      | PF.isDirectory       fs = return $ Dir       fn' fv
      | PF.isRegularFile     fs = return $ RegFile   fn' fv
      | PF.isBlockDevice     fs = return $ BlockDev  fn' fv
      | PF.isCharacterDevice fs = return $ CharDev   fn' fv
      | PF.isNamedPipe       fs = return $ NamedPipe fn' fv
      | PF.isSocket          fs = return $ Socket    fn' fv
      | otherwise               = return $ Failed    fn' (userError
                                                          "Unknown filetype!")

-- |Reads a file via `readFile` and fills the free variable via `getFileInfo`.
readFileWithFileInfo :: Path Abs -> IO (AnchoredFile FileInfo)
readFileWithFileInfo = readFile getFileInfo


-- |Same as readDirectoryContents but allows us to, for example, use
-- ByteString.readFile to return a tree of ByteStrings.
readDirectoryContents :: (Path Abs -> IO [Path Fn])
                      -> (Path Abs -> IO a)
                      -> Path Abs
                      -> IO [AnchoredFile a]
readDirectoryContents getfiles ff p = do
  files <- getfiles p
  fcs <- mapM (\x -> readFile ff $ p P.</> x) files
  return $ removeNonexistent fcs


-- |A variant of `readDirectoryContents` which uses `readFileUnsafe`.
-- Suitable for cases where we know the paths are safe/correct
-- and need the extra bit of performance.
readDirectoryContentsUnsafe :: (Path Abs -> IO [Path Fn])
                            -> (Path Abs -> IO a)
                            -> Path Abs
                            -> IO [AnchoredFile a]
readDirectoryContentsUnsafe getfiles ff p = do
  files <- getfiles p
  fcs <- mapM (\x -> readFileUnsafe ff $ p P.</> x) files
  return $ removeNonexistent fcs


-- |Build a list of AnchoredFile, given the path to a directory, filling
-- the free variables via `getFileInfo`. This includes the "." and ".."
-- directories.
readDirectoryContentsWithFileInfo :: Path Abs -> IO [AnchoredFile FileInfo]
readDirectoryContentsWithFileInfo fp
  = readDirectoryContents getAllDirsFiles getFileInfo fp


-- |Build a list of AnchoredFile, given the path to a directory, filling
-- the free variables via `getFileInfo`. This excludes the "." and ".."
-- directories.
readDirectoryContentsWithFileInfo' :: Path Abs -> IO [AnchoredFile FileInfo]
readDirectoryContentsWithFileInfo' fp
  = readDirectoryContents getDirsFiles getFileInfo fp


-- |Get the contents of a directory, including "." and "..".
getContents :: AnchoredFile FileInfo
            -> IO [AnchoredFile FileInfo]
getContents (ADirOrSym af) = readDirectoryContentsWithFileInfo (fullPath af)
getContents _              = return []


-- |Get the contents of a directory, including "." and "..".
getContents' :: AnchoredFile FileInfo
             -> IO [AnchoredFile FileInfo]
getContents' (ADirOrSym af) = readDirectoryContentsWithFileInfo' (fullPath af)
getContents' _              = return []


-- |Go up one directory in the filesystem hierarchy.
goUp :: AnchoredFile FileInfo -> IO (AnchoredFile FileInfo)
goUp af@(Path "" :/ _) = return af
goUp    (bp      :/ _) = readFile getFileInfo bp


-- |Go up one directory in the filesystem hierarchy.
goUp' :: Path Abs -> IO (AnchoredFile FileInfo)
goUp' fp = readFile getFileInfo $ P.dirname fp




    -----------------
    --[ UTILITIES ]--
    -----------------



---- HANDLING FAILURES ----


-- |True if any Failed constructors in the tree.
anyFailed :: [File a] -> Bool
anyFailed = not . successful

-- |True if there are no Failed constructors in the tree.
successful :: [File a] -> Bool
successful = null . failures


-- |Returns true if argument is a `Failed` constructor.
failed :: File a -> Bool
failed (Failed _ _) = True
failed _            = False


-- |Returns a list of 'Failed' constructors only.
failures :: [File a] -> [File a]
failures = filter failed



---- ORDERING AND EQUALITY ----


 -- HELPER: a non-recursive comparison
comparingConstr :: File FileInfo -> File FileInfo -> Ordering
comparingConstr (Failed _ _)      (DirOrSym _)      = LT
comparingConstr (Failed _ _)      (FileLikeOrSym _) = LT
comparingConstr (FileLikeOrSym _) (Failed _ _)      = GT
comparingConstr (FileLikeOrSym _) (DirOrSym _)      = GT
comparingConstr (DirOrSym _)      (Failed _ _)      = GT
comparingConstr (DirOrSym _)      (FileLikeOrSym _) = LT
 -- else compare on the names of constructors that are the same, without
 -- looking at the contents of Dir constructors:
comparingConstr t t'  = compare (name t) (name t')







    ---------------
    --[ HELPERS ]--
    ---------------


---- CONSTRUCTOR IDENTIFIERS ----

isFileC :: File a -> Bool
isFileC RegFile{} = True
isFileC _         = False


isDirC :: File a -> Bool
isDirC Dir{} = True
isDirC _     = False


isSymC :: File a -> Bool
isSymC SymLink{} = True
isSymC _         = False


isBlockC :: File a -> Bool
isBlockC BlockDev{} = True
isBlockC _          = False


isCharC :: File a -> Bool
isCharC CharDev{} = True
isCharC _         = False


isNamedC :: File a -> Bool
isNamedC NamedPipe{} = True
isNamedC _           = False


isSocketC :: File a -> Bool
isSocketC Socket{} = True
isSocketC _        = False




---- IO HELPERS: ----


-- |Gets all filenames of the given directory.
-- The first argument is a filter function that allows to exclude
-- filenames from the result.
getDirsFiles' :: (Path Fn -> [Path Fn] -> [Path Fn]) -- ^ filter function
              -> Path Abs                            -- ^ dir to read
              -> IO [Path Fn]
getDirsFiles' filterf fp =
  rethrowErrnoAs [eACCES] (Can'tOpenDirectory . P.fromAbs $ fp)
  $ bracket (PFD.openDirStream . P.toFilePath $ fp)
          PFD.closeDirStream
          $ \dirstream ->
            let mdirs :: [Path Fn] -> IO [Path Fn]
                mdirs dirs = do
                  -- make sure we close the directory stream in case of errors
                  -- TODO: more explicit error handling?
                  --       both the parsing and readin the stream can fail!
                  dir <- PFD.readDirStream dirstream
                  case dir of
                    "" -> return dirs
                    _  -> do
                            pdir <- P.parseFn dir
                            mdirs $ pdir `filterf` dirs
            in mdirs []


-- |Get all files of a given directory and return them as a List.
-- This includes "." and "..".
getAllDirsFiles :: Path Abs -> IO [Path Fn]
getAllDirsFiles = getDirsFiles' (:)


-- |Get all files of a given directory and return them as a List.
-- This excludes "." and "..".
getDirsFiles :: Path Abs -> IO [Path Fn]
getDirsFiles = getDirsFiles' insert
  where
    insert dir dirs = case dir of
      (Path ".")  -> dirs
      (Path "..") -> dirs
      _           -> dir : dirs


-- |Gets all file information.
getFileInfo :: Path Abs -> IO FileInfo
getFileInfo fp = do
  fs <- PF.getSymbolicLinkStatus (P.fromAbs fp)
  return $ FileInfo
    (PF.deviceID fs)
    (PF.fileID fs)
    (PF.fileMode fs)
    (PF.linkCount fs)
    (PF.fileOwner fs)
    (PF.fileGroup fs)
    (PF.specialDeviceID fs)
    (PF.fileSize fs)
    (PF.accessTime fs)
    (PF.modificationTime fs)
    (PF.statusChangeTime fs)
    (PF.accessTimeHiRes fs)
    (PF.modificationTimeHiRes fs)
    (PF.statusChangeTimeHiRes fs)



---- FAILURE HELPERS: ----


-- Handles an IO exception by returning a Failed constructor filled with that
-- exception. Does not handle FmIOExceptions.
handleDT :: Path Abs
         -> Path Fn
         -> IO (AnchoredFile a)
         -> IO (AnchoredFile a)
handleDT bp n
  = handleIOError $ \e -> return $ bp :/ Failed n e


-- DoesNotExist errors not present at the topmost level could happen if a
-- named file or directory is deleted after being listed by
-- getDirectoryContents but before we can get it into memory.
--    So we filter those errors out because the user should not see errors
-- raised by the internal implementation of this module:
--    This leaves the error if it exists in the top (user-supplied) level:
removeNonexistent :: [AnchoredFile a] -> [AnchoredFile a]
removeNonexistent = filter isOkConstructor
  where
    isOkConstructor (_ :/ c) = not (failed c) || isOkError c
    isOkError = not . isDoesNotExistErrorType . ioeGetErrorType . err


---- SYMLINK HELPERS: ----


-- |Checks if a symlink is broken by examining the constructor of the
-- symlink destination.
--
-- When called on a non-symlink, returns False.
isBrokenSymlink :: File FileInfo -> Bool
isBrokenSymlink (SymLink _ _ (_ :/ Failed {}) _) = True
isBrokenSymlink _ = False


---- OTHER: ----


-- |Apply a function on the free variable. If there is no free variable
-- for the given constructor the value from the `Default` class is used.
fromFreeVar :: (Default d) => (a -> d) -> File a -> d
fromFreeVar f df = maybeD f $ getFreeVar df


-- |Gets the free variable. Returns Nothing if the constructor is of `Failed`.
getFreeVar :: File a -> Maybe a
getFreeVar (Dir _  d)         = Just d
getFreeVar (RegFile _  d)     = Just d
getFreeVar (SymLink _  d _ _) = Just d
getFreeVar (BlockDev _  d)    = Just d
getFreeVar (CharDev _  d)     = Just d
getFreeVar (NamedPipe _  d)   = Just d
getFreeVar (Socket _  d)      = Just d
getFreeVar _                  = Nothing


-- |Get the full path of the file.
fullPath :: AnchoredFile a -> Path Abs
fullPath (bp :/ f) = bp P.</> name f


-- |Get the full path of the file, converted to a `FilePath`.
fullPathS :: AnchoredFile a -> ByteString
fullPathS = P.fromAbs . fullPath


-- |Pack the modification time into a string.
packModTime :: File FileInfo
            -> String
packModTime =
  fromFreeVar
    $ show . posixSecondsToUTCTime . realToFrac . modificationTime


-- |Pack the permissions into a string, similar to what "ls -l" does.
packPermissions :: File FileInfo
                -> String
packPermissions dt = fromFreeVar (pStr . fileMode) dt
  where
    pStr :: FileMode -> String
    pStr ffm = typeModeStr ++ ownerModeStr ++ groupModeStr ++ otherModeStr
      where
        typeModeStr = case dt of
          Dir {}       -> "d"
          RegFile {}   -> "-"
          SymLink {}   -> "l"
          BlockDev {}  -> "b"
          CharDev {}   -> "c"
          NamedPipe {} -> "p"
          Socket {}    -> "s"
          _            -> "?"
        ownerModeStr =    hasFmStr PF.ownerReadMode    "r"
                       ++ hasFmStr PF.ownerWriteMode   "w"
                       ++ hasFmStr PF.ownerExecuteMode "x"
        groupModeStr =    hasFmStr PF.groupReadMode    "r"
                       ++ hasFmStr PF.groupWriteMode   "w"
                       ++ hasFmStr PF.groupExecuteMode "x"
        otherModeStr =    hasFmStr PF.otherReadMode    "r"
                       ++ hasFmStr PF.otherWriteMode   "w"
                       ++ hasFmStr PF.otherExecuteMode "x"
        hasFmStr fm str
          | hasFM fm  = str
          | otherwise = "-"
        hasFM fm = ffm `PF.intersectFileModes` fm == fm

