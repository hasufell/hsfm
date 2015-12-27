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

-- |This module provides data types for representing directories/files
-- and related operations on it, mostly internal stuff, not actual IO actions.
--
-- It doesn't allow to represent the whole filesystem, since that's only
-- possible through IO laziness, which introduces too much internal state.
module Data.DirTree where


import Control.Applicative
  (
    (<*>)
  , (<$>)
  , (<|>)
  , pure
  )
import Control.Arrow
  (
    first
  )
import Control.Exception
  (
    handle
  )
import Control.Exception.Base
  (
    onException
  , IOException
  )
import Control.Monad.State.Lazy
  (

  )
import Data.Default
import Data.List
  (
    delete
  , foldl'
  , isPrefixOf
  , sort
  , sortBy
  , (\\)
  )
import Data.Maybe
  (
    fromMaybe
  )
import Data.Ord
  (
    comparing
  )
import Data.Time.Clock.POSIX
  (
    POSIXTime
  , posixSecondsToUTCTime
  )
import Data.Time
  (
    UTCTime(..)
  )
import Data.Traversable
  (
    for
  )
import Data.Word
  (
    Word64
  )
import Safe
  (
    atDef
  , initDef
  )
import System.FilePath
  (
    combine
  , normalise
  , equalFilePath
  , isAbsolute
  , joinPath
  , pathSeparator
  , splitDirectories
  , takeFileName
  , (</>)
  )
import System.IO
  (
    IOMode
  , Handle
  , openFile
  )
import System.IO.Error
  (
    ioeGetErrorType
  , isDoesNotExistErrorType
  )
import System.IO.Unsafe
  (
    unsafeInterleaveIO
  )
import System.Locale
  (
    defaultTimeLocale
  , rfc822DateFormat
  )
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

import qualified System.Posix.Files as PF
import qualified System.Posix.Directory as PFD



    ----------------------------
    --[ BASE TYPES ]--
    ----------------------------


-- |Weak type to distinguish between FilePath and FileName.
type FileName = String


-- |Represents a file. The `anchor` field is the path
-- to that file without the filename.
data AnchoredFile a =
  (:/) { anchor :: FilePath, file :: File a }
  deriving (Eq, Show)


-- |The String in the name field is always a file name, never a full path.
-- The free type variable is used in the File/Dir constructor and can hold
-- Handles, Strings representing a file's contents or anything else you can
-- think of. We catch any IO errors in the Failed constructor. an Exception
-- can be converted to a String with 'show'.
data File a =
    Failed {
    name :: FileName
  , err  :: IOException
  }
  | Dir {
    name :: FileName
  , fvar :: a
  }
  | RegFile {
    name :: FileName
  , fvar :: a
  }
  | SymLink {
    name  :: FileName
  , fvar  :: a
  , sdest :: AnchoredFile a  -- ^ symlink madness,
                             --   we need to know where it points to
  }
  | BlockDev {
    name :: FileName
  , fvar :: a
  }
  | CharDev {
    name :: FileName
  , fvar :: a
  }
  | NamedPipe {
    name :: FileName
  , fvar :: a
  }
  | Socket {
    name :: FileName
  , fvar :: a
  } deriving (Show, Eq)


-- |All possible file information we could ever need.
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


type UserIO a = FilePath -> IO a

type Builder a = UserIO a -> FilePath -> IO [File a]




    ------------------------------------
    --[ ViewPatterns/PatternSynonyms ]--
    ------------------------------------


convertViewP :: (File FileInfo -> (Bool, File FileInfo))
             -> AnchoredFile FileInfo
             -> (Bool, AnchoredFile FileInfo)
convertViewP f af@(bp :/ constr) =
  let (b, file) = f constr
  in (b, bp :/ file)


afileLike :: AnchoredFile FileInfo -> (Bool, AnchoredFile FileInfo)
afileLike f@(bp :/ constr) = convertViewP fileLike f

fileLike :: File FileInfo -> (Bool, File FileInfo)
fileLike f@(RegFile {})   = (True, f)
fileLike f@(BlockDev {})  = (True, f)
fileLike f@(CharDev {})   = (True, f)
fileLike f@(NamedPipe {}) = (True, f)
fileLike f@(Socket {})    = (True, f)
fileLike f                = (False, f)


sadir :: AnchoredFile FileInfo -> (Bool, AnchoredFile FileInfo)
sadir f = convertViewP sdir f

sdir :: File FileInfo -> (Bool, File FileInfo)
sdir f@(SymLink { sdest = (_ :/ s@(SymLink {}) )})
  -- we have to follow a chain of symlinks here, but
  -- return only the very first level
  = case (sdir s) of
      (True, _) -> (True, f)
      _         -> (False, f)
sdir f@(SymLink { sdest = (_ :/ Dir {} )})
  = (True, f)
sdir f@(Dir {}) = (True, f)
sdir f          = (False, f)


safileLike :: AnchoredFile FileInfo -> (Bool, AnchoredFile FileInfo)
safileLike f = convertViewP sfileLike f

sfileLike :: File FileInfo -> (Bool, File FileInfo)
sfileLike f@(RegFile {})   = (True, f)
sfileLike f@(BlockDev {})  = (True, f)
sfileLike f@(CharDev {})   = (True, f)
sfileLike f@(NamedPipe {}) = (True, f)
sfileLike f@(Socket {})    = (True, f)
sfileLike f                = fileLikeSym f


afileLikeSym :: AnchoredFile FileInfo -> (Bool, AnchoredFile FileInfo)
afileLikeSym f = convertViewP fileLikeSym f

fileLikeSym :: File FileInfo -> (Bool, File FileInfo)
fileLikeSym f@(SymLink { sdest = (_ :/ s@(SymLink {}) )})
  = case (fileLikeSym s) of
      (True, _) -> (True, f)
      _         -> (False, f)
fileLikeSym f@(SymLink { sdest = (_ :/ RegFile {} )})   = (True, f)
fileLikeSym f@(SymLink { sdest = (_ :/ BlockDev {} )})  = (True, f)
fileLikeSym f@(SymLink { sdest = (_ :/ CharDev {} )})   = (True, f)
fileLikeSym f@(SymLink { sdest = (_ :/ NamedPipe {} )}) = (True, f)
fileLikeSym f@(SymLink { sdest = (_ :/ Socket {} )})    = (True, f)
fileLikeSym f                                           = (False, f)


adirSym :: AnchoredFile FileInfo -> (Bool, AnchoredFile FileInfo)
adirSym f = convertViewP dirSym f

dirSym :: File FileInfo -> (Bool, File FileInfo)
dirSym f@(SymLink { sdest = (_ :/ s@(SymLink {}) )})
  = case (dirSym s) of
      (True, _) -> (True, f)
      _         -> (False, f)
dirSym f@(SymLink { sdest = (_ :/ Dir {} )}) = (True, f)
dirSym f = (False, f)


invalidFileName :: FileName -> (Bool, FileName)
invalidFileName ""   = (True, "")
invalidFileName "."  = (True, ".")
invalidFileName ".." = (True, "..")
invalidFileName fn   = (elem pathSeparator fn, fn)


abrokenSymlink :: AnchoredFile FileInfo -> (Bool, AnchoredFile FileInfo)
abrokenSymlink f = convertViewP brokenSymlink f

brokenSymlink :: File FileInfo -> (Bool, File FileInfo)
brokenSymlink f = (isBrokenSymlink f, f)


-- |Matches on invalid filesnames, such as ".", ".." and anything
-- that contains a path separator.
pattern InvFN   <- (invalidFileName -> (True,_))
-- |Opposite of `InvFN`.
pattern ValFN f <- (invalidFileName -> (False, f))

pattern AFileInvFN  <- (fst . invalidFileName . name . file -> True)
pattern FileInvFN   <- (fst . invalidFileName . name        -> True)


-- |Matches on directories or symlinks pointing to directories.
-- If the symlink is pointing to a symlink pointing to a directory, then
-- it will return True, but also return the first element in the symlink-
-- chain, not the last.
pattern ADirOrSym f <- (sadir -> (True, f))
pattern DirOrSym  f <- (sdir  -> (True, f))

-- |Matches on symlinks pointing to directories only.
pattern ADirSym  f <- (adirSym -> (True, f))
pattern DirSym   f <- (dirSym  -> (True, f))

-- |Matches on any non-directory kind of files or symlinks pointing to
-- such.
-- If the symlink is pointing to a symlink pointing to such a file, then
-- it will return True, but also return the first element in the symlink-
-- chain, not the last.
pattern AFileLikeOrSym f <- (safileLike -> (True, f))
pattern FileLikeOrSym  f <- (sfileLike  -> (True, f))

-- |Matches on any non-directory kind of files, excluding symlinks.
pattern AFileLike f <- (afileLike -> (True, f))
pattern FileLike  f <- (fileLike  -> (True, f))

-- |Matches on symlinks pointing to file-like files only.
pattern AFileLikeSym f <- (afileLikeSym -> (True, f))
pattern FileLikeSym  f <- (fileLikeSym  -> (True, f))

-- |Matches on broken symbolic links.
pattern ABrokenSymlink f <- (abrokenSymlink -> (True, f))
pattern BrokenSymlink  f <- (brokenSymlink  -> (True, f))

-- |Matches a list of directories or symlinks pointing to directories.
pattern DirList fs <- (\fs -> (foldr (&&) True . fmap (fst . sadir) $ fs, fs)
                                -> (True, fs))
-- |Matches a list of any non-directory kind of files or symlinks
-- pointing to such.
pattern FileLikeList fs <- (\fs -> (foldr (&&) True
                                     . fmap (fst . safileLike)
                                     $ fs, fs) -> (True, fs))




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


-- |Read a file into an `AnchoredFile`, filling the free variables via
-- the given function.
readFileWith :: (FilePath -> IO a)
             -> FilePath
             -> IO (AnchoredFile a)
readFileWith ff p = do
  let fn = topDir p
      bd = baseDir p
  handleDT' bd fn $ do
    fs  <- PF.getSymbolicLinkStatus p
    fv  <- ff p
    file <- constructFile fs fv bd fn
    return (bd :/ file)
  where
    constructFile fs fv bd' n
      | PF.isSymbolicLink    fs = do
          -- symlink madness, we need to make sure we save the correct
          -- AnchoredFile
          let fp = bd' </> n
          resolvedSyml <- handleDT' bd' n $ do
            sfp <- (\x -> if isAbsolute x then x else bd' </> x)
                     <$> PF.readSymbolicLink fp
            _ <- PF.getFileStatus sfp -- important to break infinite symbolic
                                      -- link cycle
            readFileWith ff sfp
          return $ SymLink n fv resolvedSyml
      | PF.isDirectory       fs = return $ Dir       n fv
      | PF.isRegularFile     fs = return $ RegFile   n fv
      | PF.isBlockDevice     fs = return $ BlockDev  n fv
      | PF.isCharacterDevice fs = return $ CharDev   n fv
      | PF.isNamedPipe       fs = return $ NamedPipe n fv
      | PF.isSocket          fs = return $ Socket    n fv
      | otherwise               = return $ Failed    n (userError
                                                        "Unknown filetype!")


readFile :: FilePath -> IO (AnchoredFile FileInfo)
readFile fp = readFileWith getFileInfo $ normalize fp


-- |Build a list of AnchoredFile, given the path to a directory, filling
-- the free variables via `getFileInfo`. This includes the "." and ".."
-- directories.
readDirectory :: FilePath -> IO [AnchoredFile FileInfo]
readDirectory fp = readDirectoryWith getAllDirsFiles getFileInfo
                     $ normalize fp


-- |Build a list of AnchoredFile, given the path to a directory, filling
-- the free variables via `getFileInfo`. This excludes the "." and ".."
-- directories.
readDirectory' :: FilePath -> IO [AnchoredFile FileInfo]
readDirectory' fp = readDirectoryWith getDirsFiles getFileInfo
                      $ normalize fp


-- | same as readDirectory but allows us to, for example, use
-- ByteString.readFile to return a tree of ByteStrings.
readDirectoryWith :: (FilePath -> IO [FilePath])
                  -> (FilePath -> IO a)
                  -> FilePath
                  -> IO [AnchoredFile a]
readDirectoryWith getfiles ff p = do
  contents <- getfiles $ normalize p
  cs <- mapM (\x -> readFileWith ff $ p </> x) contents
  return $ removeNonexistent cs





    -----------------
    --[ UTILITIES ]--
    -----------------



---- HANDLING FAILURES ----


-- | True if any Failed constructors in the tree
anyFailed :: [File a] -> Bool
anyFailed = not . successful

-- | True if there are no Failed constructors in the tree
successful :: [File a] -> Bool
successful = null . failures


-- | returns true if argument is a `Failed` constructor:
failed :: File a -> Bool
failed (Failed _ _) = True
failed _            = False


-- | returns a list of 'Failed' constructors only:
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




---- OTHER ----



    ---------------
    --[ HELPERS ]--
    ---------------


---- CONSTRUCTOR IDENTIFIERS ----

isFileC :: File a -> Bool
isFileC (RegFile _ _) = True
isFileC _             = False


isDirC :: File a -> Bool
isDirC (Dir _ _) = True
isDirC _         = False


isSymC :: File a -> Bool
isSymC (SymLink _ _ _) = True
isSymC _               = False


isBlockC :: File a -> Bool
isBlockC (BlockDev _ _) = True
isBlockC _              = False


isCharC :: File a -> Bool
isCharC (CharDev _ _) = True
isCharC _             = False



isNamedC :: File a -> Bool
isNamedC (NamedPipe _ _) = True
isNamedC _               = False


isSocketC :: File a -> Bool
isSocketC (Socket _ _) = True
isSocketC _            = False



---- PATH CONVERSIONS ----



-- extracting pathnames and base names:
topDir, baseDir :: FilePath -> FilePath
topDir = last . splitDirectories
baseDir = joinPath . init . splitDirectories


-- |Check whether the given file is a hidden file.
hiddenFile :: FilePath -> Bool
hiddenFile "."  = False
hiddenFile ".." = False
hiddenFile str
  | "." `isPrefixOf` str = True
  | otherwise            = False


-- |Like `normalise` from System.FilePath but removes occurences of '..'.
-- Note that this sort of misbehaves if the path contains symlink
-- components.
normalize :: FilePath -> FilePath
normalize fp =
  joinPath $ foldl' ff [] (splitDirectories . normalise $ fp)
  where
    ff ["/"] ".." = ["/"]
    ff x ".."     = initDef [] x
    ff x y        = x ++ [y]



---- IO HELPERS: ----


-- |Go up one directory in the filesystem hierarchy.
goUp :: AnchoredFile FileInfo -> IO (AnchoredFile FileInfo)
goUp af@("" :/ _) = return af
goUp    (bp :/ _) = Data.DirTree.readFile bp


-- |Go up one directory in the filesystem hierarchy.
goUp' :: FilePath -> IO (AnchoredFile FileInfo)
goUp' fp = do
  let cfp = normalize fp
  Data.DirTree.readFile $ baseDir cfp


-- |Get the contents of a directory.
getContents :: AnchoredFile FileInfo
            -> IO [AnchoredFile FileInfo]
getContents (ADirOrSym af) = readDirectory (fullPath af)
getContents _              = return []


getDirsFiles' :: (FilePath -> [FilePath] -> [FilePath])
              -> FilePath
              -> IO [FilePath]
getDirsFiles' filterf fp = do
  dirstream <- PFD.openDirStream fp
  let mdirs :: [FilePath] -> IO [FilePath]
      mdirs dirs = do
        -- make sure we close the directory stream in case of errors
        dir <- onException (PFD.readDirStream dirstream)
                           (PFD.closeDirStream dirstream)
        if dir == ""
          then return dirs
          else mdirs (dir `filterf` dirs)
  dirs <- mdirs []
  PFD.closeDirStream dirstream
  return dirs


-- |Get all files of a given directory and return them as a List.
-- This includes "." and "..".
getAllDirsFiles :: FilePath -> IO [FilePath]
getAllDirsFiles = getDirsFiles' (:)


-- |Get all files of a given directory and return them as a List.
-- This excludes "." and "..".
getDirsFiles :: FilePath -> IO [FilePath]
getDirsFiles = getDirsFiles' insert
  where
    insert dir dirs = case dir of
      "."  -> dirs
      ".." -> dirs
      _    -> dir : dirs


-- |Gets all file information.
getFileInfo :: FilePath -> IO FileInfo
getFileInfo fp = do
  fs <- PF.getSymbolicLinkStatus fp
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


-- |Gets the free variable. Returns Nothing if the constructor is of `Failed`.
getFreeVar :: File a -> Maybe a
getFreeVar (Dir _  d)       = Just d
getFreeVar (RegFile _  d)   = Just d
getFreeVar (SymLink _  d _) = Just d
getFreeVar (BlockDev _  d)  = Just d
getFreeVar (CharDev _  d)   = Just d
getFreeVar (NamedPipe _  d) = Just d
getFreeVar (Socket _  d)    = Just d
getFreeVar _                = Nothing


---- FAILURE HELPERS: ----


-- handles an IO exception by returning a Failed constructor filled with that
-- exception:
handleDT :: FileName -> IO (File a) -> IO (File a)
handleDT n = handle (return . Failed n)


-- handles an IO exception by returning a Failed constructor filled with that
-- exception:
handleDT' :: FilePath -> FileName -> IO (AnchoredFile a) -> IO (AnchoredFile a)
handleDT' bp n = handle (\e -> return $ bp :/ Failed n e)


-- DoesNotExist errors not present at the topmost level could happen if a
-- named file or directory is deleted after being listed by
-- getDirectoryContents but before we can get it into memory.
--    So we filter those errors out because the user should not see errors
-- raised by the internal implementation of this module:
--     This leaves the error if it exists in the top (user-supplied) level:
removeNonexistent :: [AnchoredFile a] -> [AnchoredFile a]
removeNonexistent = filter isOkConstructor
  where
    isOkConstructor (_ :/ c) = not (failed c) || isOkError c
    isOkError = not . isDoesNotExistErrorType . ioeGetErrorType . err


---- SYMLINK HELPERS: ----


-- |Follows a chain of symlinks until it finds a non-symlink. Note that
-- this can be caught in an infinite loop if the symlinks haven't been
-- constructed properly. This module however ensures that this cannot
-- happen.
followSymlink :: File FileInfo -> File FileInfo
followSymlink (SymLink _ _ (_ :/ b@(SymLink {}))) = followSymlink b
followSymlink af                                  = af


-- |Checks if a symlink is broken by examining the constructor of the
-- symlink destination. This also follows the symlink chain.
--
-- When called on a non-symlink, returns False.
isBrokenSymlink :: File FileInfo -> Bool
isBrokenSymlink af@(SymLink _ _ (_ :/ Failed {})) = True
isBrokenSymlink af@(SymLink {})
  = case followSymlink af of
      (Failed {}) -> True
      _           -> False
isBrokenSymlink _ = False


---- OTHER: ----



fullPath :: AnchoredFile a -> FilePath
fullPath (bp :/ f) = bp </> name f


-- |Apply a function on the free variable. If there is no free variable
-- for the given constructor the value from the `Default` class is used.
fromFreeVar :: (Default d) => (a -> d) -> File a -> d
fromFreeVar f df = maybeD f $ getFreeVar df


-- |A `maybe` flavor using the `Default` class.
maybeD :: (Default b) => (a -> b) -> Maybe a -> b
maybeD = maybe def


-- |Pack the modification time into a string.
packModTime :: File FileInfo
            -> String
packModTime = fromFreeVar
                $ show . posixSecondsToUTCTime . realToFrac . modificationTime


-- |Pack the permissions into a string, similar to what "ls -l" does.
packPermissions :: File FileInfo
                -> String
packPermissions dt = fromFreeVar (pStr . fileMode) dt
  where
    pStr ffm = typeModeStr ++ ownerModeStr ++ groupModeStr ++ otherModeStr
      where
        typeModeStr
          | hasFM PF.regularFileMode      = "-"
          | hasFM PF.directoryMode        = "d"
          | hasFM PF.symbolicLinkMode     = "l"
          | hasFM PF.socketMode           = "s"
          | hasFM PF.blockSpecialMode     = "b"
          | hasFM PF.characterSpecialMode = "c"
          | hasFM PF.namedPipeMode        = "p"
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
