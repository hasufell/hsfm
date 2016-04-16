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
import System.Posix.FilePath
  (
    (</>)
  )
import System.Posix.Directory.Traversals (realpath)
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


-- |The String in the path field is always a full path.
-- The free type variable is used in the File/Dir constructor and can hold
-- Handles, Strings representing a file's contents or anything else you can
-- think of. We catch any IO errors in the Failed constructor. an Exception
-- can be converted to a String with 'show'.
data File a =
    Failed {
    path :: Path Abs
  , err  :: IOError
  }
  | Dir {
    path :: Path Abs
  , fvar :: a
  }
  | RegFile {
    path :: Path Abs
  , fvar :: a
  }
  | SymLink {
    path    :: Path Abs
  , fvar    :: a
  , sdest   :: File a  -- ^ symlink madness,
                       --   we need to know where it points to
  , rawdest :: ByteString
  }
  | BlockDev {
    path :: Path Abs
  , fvar :: a
  }
  | CharDev {
    path :: Path Abs
  , fvar :: a
  }
  | NamedPipe {
    path :: Path Abs
  , fvar :: a
  }
  | Socket {
    path :: Path Abs
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




---- Filetypes ----


sfileLike :: File FileInfo -> (Bool, File FileInfo)
sfileLike f@RegFile{}   = (True, f)
sfileLike f@BlockDev{}  = (True, f)
sfileLike f@CharDev{}   = (True, f)
sfileLike f@NamedPipe{} = (True, f)
sfileLike f@Socket{}    = (True, f)
sfileLike f             = fileLikeSym f


fileLike :: File FileInfo -> (Bool, File FileInfo)
fileLike f@RegFile {}  = (True, f)
fileLike f@BlockDev{}  = (True, f)
fileLike f@CharDev{}   = (True, f)
fileLike f@NamedPipe{} = (True, f)
fileLike f@Socket{}    = (True, f)
fileLike f             = (False, f)


sdir :: File FileInfo -> (Bool, File FileInfo)
sdir f@SymLink{ sdest = (s@SymLink{} )}
  -- we have to follow a chain of symlinks here, but
  -- return only the very first level
  -- TODO: this is probably obsolete now
  = case sdir s of
      (True, _) -> (True, f)
      _         -> (False, f)
sdir f@SymLink{ sdest = Dir{} }
  = (True, f)
sdir f@Dir{} = (True, f)
sdir f       = (False, f)


-- |Matches on any non-directory kind of files, excluding symlinks.
pattern FileLike  f <- (fileLike  -> (True, f))

-- |Matches a list of directories or symlinks pointing to directories.
pattern DirList fs <- (\fs -> (and . fmap (fst . sdir) $ fs, fs)
                                -> (True, fs))

-- |Matches a list of any non-directory kind of files or symlinks
-- pointing to such.
pattern FileLikeList fs <- (\fs -> (and
                                     . fmap (fst . sfileLike)
                                     $ fs, fs) -> (True, fs))



---- Symlinks ----


brokenSymlink :: File FileInfo -> (Bool, File FileInfo)
brokenSymlink f = (isBrokenSymlink f, f)


fileLikeSym :: File FileInfo -> (Bool, File FileInfo)
fileLikeSym f@SymLink{ sdest = s@SymLink{} }
  = case fileLikeSym s of
      (True, _) -> (True, f)
      _         -> (False, f)
fileLikeSym f@SymLink{ sdest = RegFile{} }   = (True, f)
fileLikeSym f@SymLink{ sdest = BlockDev{} }  = (True, f)
fileLikeSym f@SymLink{ sdest = CharDev{} }   = (True, f)
fileLikeSym f@SymLink{ sdest = NamedPipe{} } = (True, f)
fileLikeSym f@SymLink{ sdest = Socket{} }    = (True, f)
fileLikeSym f                                = (False, f)


dirSym :: File FileInfo -> (Bool, File FileInfo)
dirSym f@SymLink{ sdest = s@SymLink{} }
  = case dirSym s of
      (True, _) -> (True, f)
      _         -> (False, f)
dirSym f@SymLink{ sdest = Dir{} } = (True, f)
dirSym f = (False, f)


-- |Matches on symlinks pointing to file-like files only.
pattern FileLikeSym  f <- (fileLikeSym  -> (True, f))

-- |Matches on broken symbolic links.
pattern BrokenSymlink  f <- (brokenSymlink  -> (True, f))


-- |Matches on directories or symlinks pointing to directories.
-- If the symlink is pointing to a symlink pointing to a directory, then
-- it will return True, but also return the first element in the symlink-
-- chain, not the last.
pattern DirOrSym  f <- (sdir  -> (True, f))

-- |Matches on symlinks pointing to directories only.
pattern DirSym   f <- (dirSym  -> (True, f))

-- |Matches on any non-directory kind of files or symlinks pointing to
-- such.
-- If the symlink is pointing to a symlink pointing to such a file, then
-- it will return True, but also return the first element in the symlink-
-- chain, not the last.
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





    ----------------------------
    --[ HIGH LEVEL FUNCTIONS ]--
    ----------------------------



-- |Reads a file or directory Path into an `AnchoredFile`, filling the free
-- variables via the given function.
readFile :: (Path Abs -> IO a)
         -> Path Abs
         -> IO (File a)
readFile ff p =
  handleDT p $ do
    fs   <- PF.getSymbolicLinkStatus (P.toFilePath p)
    fv   <- ff p
    constructFile fs fv p
  where
    constructFile fs fv p'
      | PF.isSymbolicLink    fs = do
          -- symlink madness, we need to make sure we save the correct
          -- File
          x <- PF.readSymbolicLink (P.fromAbs p')
          resolvedSyml <- handleDT p' $ do
            -- watch out, we call </> from 'filepath' here, but it is safe
            let sfp = (P.fromAbs . P.dirname $ p') </> x
            rsfp <- realpath sfp
            readFile ff =<< P.parseAbs rsfp
          return $ SymLink p' fv resolvedSyml x
      | PF.isDirectory       fs = return $ Dir       p' fv
      | PF.isRegularFile     fs = return $ RegFile   p' fv
      | PF.isBlockDevice     fs = return $ BlockDev  p' fv
      | PF.isCharacterDevice fs = return $ CharDev   p' fv
      | PF.isNamedPipe       fs = return $ NamedPipe p' fv
      | PF.isSocket          fs = return $ Socket    p' fv
      | otherwise               = return $ Failed    p' (userError
                                                         "Unknown filetype!")


-- |Get the contents of a given directory and return them as a list
-- of `AnchoredFile`.
readDirectoryContents :: (Path Abs -> IO a)  -- ^ fills free a variable
                      -> Path Abs            -- ^ path to read
                      -> IO [File a]
readDirectoryContents ff p = do
  files <- getDirsFiles p
  fcs   <- mapM (readFile ff) files
  return $ removeNonexistent fcs


-- |A variant of `readDirectoryContents` where the third argument
-- is a `File`. If a non-directory is passed returns an empty list.
getContents :: (Path Abs -> IO a)
            -> File FileInfo
            -> IO [File a]
getContents ff (DirOrSym af)
  = readDirectoryContents ff (fullPath af)
getContents _ _ = return []



-- |Go up one directory in the filesystem hierarchy.
goUp :: File FileInfo -> IO (File FileInfo)
goUp file = readFile getFileInfo (P.dirname . path $ file)


-- |Go up one directory in the filesystem hierarchy.
goUp' :: Path Abs -> IO (File FileInfo)
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
comparingConstr t t'  = compare (path t) (path t')







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


-- |Gets all filenames of the given directory. This excludes "." and "..".
getDirsFiles :: Path Abs        -- ^ dir to read
             -> IO [Path Abs]
getDirsFiles fp =
  rethrowErrnoAs [eACCES] (Can'tOpenDirectory . P.fromAbs $ fp)
  $ bracket (PFD.openDirStream . P.toFilePath $ fp)
          PFD.closeDirStream
          $ \dirstream ->
            let mdirs :: [Path Abs] -> IO [Path Abs]
                mdirs dirs = do
                  -- make sure we close the directory stream in case of errors
                  -- TODO: more explicit error handling?
                  --       both the parsing and readin the stream can fail!
                  dir <- PFD.readDirStream dirstream
                  if B.null dir
                    then return dirs
                    else mdirs $ maybe dirs
                                       (\x -> fp P.</> x : dirs)
                                       (P.parseFn dir)
            in mdirs []


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
         -> IO (File a)
         -> IO (File a)
handleDT p
  = handleIOError $ \e -> return $ Failed p e


-- DoesNotExist errors not present at the topmost level could happen if a
-- named file or directory is deleted after being listed by
-- getDirectoryContents but before we can get it into memory.
--    So we filter those errors out because the user should not see errors
-- raised by the internal implementation of this module:
--    This leaves the error if it exists in the top (user-supplied) level:
removeNonexistent :: [File a] -> [File a]
removeNonexistent = filter isOkConstructor
  where
    isOkConstructor c = not (failed c) || isOkError c
    isOkError = not . isDoesNotExistErrorType . ioeGetErrorType . err


---- SYMLINK HELPERS: ----


-- |Checks if a symlink is broken by examining the constructor of the
-- symlink destination.
--
-- When called on a non-symlink, returns False.
isBrokenSymlink :: File FileInfo -> Bool
isBrokenSymlink (SymLink _ _ Failed{} _) = True
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
fullPath :: File a -> Path Abs
fullPath f = path f


-- |Get the full path of the file, converted to a `FilePath`.
fullPathS :: File a -> ByteString
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

