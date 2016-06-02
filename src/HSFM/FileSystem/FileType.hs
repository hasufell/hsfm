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


-- |This module provides a data type for representing directories/files
-- in a well-typed and convenient way. This is useful to gather and
-- save information about a file, so the information can be easily
-- processed in e.g. a GUI.
--
-- However, it's not meant to be used to interact with low-level
-- functions that copy files etc, since there's no guarantee that
-- the in-memory representation of the type still matches what is
-- happening on filesystem level.
--
-- If you interact with low-level libraries, you must not pattern
-- match on the `File a` type. Instead, you should only use the saved
-- `path` and make no assumptions about the file the path might or
-- might not point to.
module HSFM.FileSystem.FileType where



import Data.ByteString(ByteString)
import Data.ByteString.UTF8
  (
    toString
  )
import Data.Default
import Data.Time.Clock.POSIX
  (
    POSIXTime
  , posixSecondsToUTCTime
  )
import Data.Time()
import HPath
  (
    Abs
  , Path
  )
import qualified HPath as P
import HPath.IO hiding (FileType(..))
import HPath.IO.Errors
import HSFM.Utils.MyPrelude
import Prelude hiding(readFile)
import System.Posix.FilePath
  (
    (</>)
  )
import System.Posix.Directory.Traversals
  (
    realpath
  )
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
-- think of.
data File a =
  Dir {
    path :: !(Path Abs)
  , fvar :: a
  }
  | RegFile {
    path :: !(Path Abs)
  , fvar :: a
  }
  | SymLink {
    path    :: !(Path Abs)
  , fvar    :: a
  , sdest   :: Maybe (File a)  -- ^ symlink madness,
                               --   we need to know where it points to
  , rawdest :: !ByteString
  }
  | BlockDev {
    path :: !(Path Abs)
  , fvar :: a
  }
  | CharDev {
    path :: !(Path Abs)
  , fvar :: a
  }
  | NamedPipe {
    path :: !(Path Abs)
  , fvar :: a
  }
  | Socket {
    path :: !(Path Abs)
  , fvar :: a
  } deriving (Show, Eq)


-- |Low-level file information.
data FileInfo = FileInfo {
    deviceID              :: !DeviceID
  , fileID                :: !FileID
  , fileMode              :: !FileMode
  , linkCount             :: !LinkCount
  , fileOwner             :: !UserID
  , fileGroup             :: !GroupID
  , specialDeviceID       :: !DeviceID
  , fileSize              :: !FileOffset
  , accessTime            :: !EpochTime
  , modificationTime      :: !EpochTime
  , statusChangeTime      :: !EpochTime
  , accessTimeHiRes       :: !POSIXTime
  , modificationTimeHiRes :: !POSIXTime
  , statusChangeTimeHiRes :: !POSIXTime
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
sdir f@SymLink{ sdest = (Just s@SymLink{} )}
  -- we have to follow a chain of symlinks here, but
  -- return only the very first level
  -- TODO: this is probably obsolete now
  = case sdir s of
      (True, _) -> (True, f)
      _         -> (False, f)
sdir f@SymLink{ sdest = Just Dir{} }
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
fileLikeSym f@SymLink{ sdest = Just s@SymLink{} }
  = case fileLikeSym s of
      (True, _) -> (True, f)
      _         -> (False, f)
fileLikeSym f@SymLink{ sdest = Just RegFile{} }   = (True, f)
fileLikeSym f@SymLink{ sdest = Just BlockDev{} }  = (True, f)
fileLikeSym f@SymLink{ sdest = Just CharDev{} }   = (True, f)
fileLikeSym f@SymLink{ sdest = Just NamedPipe{} } = (True, f)
fileLikeSym f@SymLink{ sdest = Just Socket{} }    = (True, f)
fileLikeSym f                                = (False, f)


dirSym :: File FileInfo -> (Bool, File FileInfo)
dirSym f@SymLink{ sdest = Just s@SymLink{} }
  = case dirSym s of
      (True, _) -> (True, f)
      _         -> (False, f)
dirSym f@SymLink{ sdest = Just Dir{} } = (True, f)
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
readFile ff p = do
    fs   <- PF.getSymbolicLinkStatus (P.toFilePath p)
    fv   <- ff p
    constructFile fs fv p
  where
    constructFile fs fv p'
      | PF.isSymbolicLink    fs = do
          -- symlink madness, we need to make sure we save the correct
          -- File
          x <- PF.readSymbolicLink (P.fromAbs p')
          resolvedSyml <- handleIOError (\_ -> return Nothing) $ do
            -- watch out, we call </> from 'filepath' here, but it is safe
            let sfp = (P.fromAbs . P.dirname $ p') </> x
            rsfp <- realpath sfp
            f <- readFile ff =<< P.parseAbs rsfp
            return $ Just f
          return $ SymLink p' fv resolvedSyml x
      | PF.isDirectory       fs = return $ Dir       p' fv
      | PF.isRegularFile     fs = return $ RegFile   p' fv
      | PF.isBlockDevice     fs = return $ BlockDev  p' fv
      | PF.isCharacterDevice fs = return $ CharDev   p' fv
      | PF.isNamedPipe       fs = return $ NamedPipe p' fv
      | PF.isSocket          fs = return $ Socket    p' fv
      | otherwise               = ioError $ userError "Unknown filetype!"


-- |Get the contents of a given directory and return them as a list
-- of `AnchoredFile`.
readDirectoryContents :: (Path Abs -> IO a)  -- ^ fills free a variable
                      -> Path Abs            -- ^ path to read
                      -> IO [File a]
readDirectoryContents ff p = do
  files <- getDirsFiles p
  mapM (readFile ff) files


-- |A variant of `readDirectoryContents` where the second argument
-- is a `File`. If a non-directory is passed returns an empty list.
getContents :: (Path Abs -> IO a)
            -> File FileInfo
            -> IO [File a]
getContents ff (DirOrSym af)
  = readDirectoryContents ff (path af)
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





---- ORDERING AND EQUALITY ----


 -- HELPER: a non-recursive comparison
comparingConstr :: File FileInfo -> File FileInfo -> Ordering
comparingConstr (FileLikeOrSym _) (DirOrSym _)      = GT
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





---- SYMLINK HELPERS: ----


-- |Checks if a symlink is broken by examining the constructor of the
-- symlink destination.
--
-- When called on a non-symlink, returns False.
isBrokenSymlink :: File FileInfo -> Bool
isBrokenSymlink (SymLink _ _ Nothing _) = True
isBrokenSymlink _ = False




---- PACKERS: ----


-- |Pack the modification time into a string.
packModTime :: File FileInfo
            -> String
packModTime = fromFreeVar $ epochToString . modificationTime


-- |Pack the modification time into a string.
packAccessTime :: File FileInfo
               -> String
packAccessTime = fromFreeVar $ epochToString . accessTime


epochToString :: EpochTime -> String
epochToString =  show . posixSecondsToUTCTime . realToFrac


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


packFileType :: File a -> String
packFileType file = case file of
  Dir {}       -> "Directory"
  RegFile {}   -> "Regular File"
  SymLink {}   -> "Symbolic Link"
  BlockDev {}  -> "Block Device"
  CharDev {}   -> "Char Device"
  NamedPipe {} -> "Named Pipe"
  Socket {}    -> "Socket"


packLinkDestination :: File a -> Maybe ByteString
packLinkDestination file = case file of
  SymLink { rawdest = dest } -> Just dest
  _                          -> Nothing




---- OTHER: ----


-- |Apply a function on the free variable. If there is no free variable
-- for the given constructor the value from the `Default` class is used.
fromFreeVar :: (Default d) => (a -> d) -> File a -> d
fromFreeVar f df = maybeD f $ getFreeVar df


getFPasStr :: File a -> String
getFPasStr = toString . P.fromAbs . path


-- |Gets the free variable. Returns Nothing if the constructor is of `Failed`.
getFreeVar :: File a -> Maybe a
getFreeVar (Dir _  d)         = Just d
getFreeVar (RegFile _  d)     = Just d
getFreeVar (SymLink _  d _ _) = Just d
getFreeVar (BlockDev _  d)    = Just d
getFreeVar (CharDev _  d)     = Just d
getFreeVar (NamedPipe _  d)   = Just d
getFreeVar (Socket _  d)      = Just d

