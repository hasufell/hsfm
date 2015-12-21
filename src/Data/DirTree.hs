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
    IOException
  )
import Control.Monad.State.Lazy
  (

  )
import Data.Default
import Data.List
  (
    delete
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
  )
import System.Directory
  (
    canonicalizePath
  , doesFileExist
  , executable
  , getPermissions
  , readable
  , searchable
  , writable
  , Permissions
  )
import System.FilePath
  (
    combine
  , equalFilePath
  , joinPath
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

import qualified Data.Bitraversable as BT
import qualified Data.Bifunctor     as BF
import qualified Data.Bifoldable    as BFL
import qualified Data.Traversable   as T
import qualified System.Posix.Files as PF
import qualified System.Posix.Directory as PFD




    ----------------------------
    --[ BASE TYPES ]--
    ----------------------------


-- |Weak type to distinguish between FilePath and FileName.
type FileName = String


-- |Represents a file. The `anchor` field is the path
-- to that file without the filename.
data AnchoredFile a b =
  (:/) { anchor :: FilePath, file :: File a b }
  deriving (Eq, Ord, Show)


-- |The String in the name field is always a file name, never a full path.
-- The free type variable is used in the File/Dir constructor and can hold
-- Handles, Strings representing a file's contents or anything else you can
-- think of. We catch any IO errors in the Failed constructor. an Exception
-- can be converted to a String with 'show'.
data File a b =
    Failed {
    name :: FileName
  , err  :: IOException
  }
  | Dir {
    name :: FileName
  , dir  :: a
  }
  | RegFile {
    name :: FileName
  , regFile :: b
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
  , isBlockDevice :: Bool
  , isCharacterDevice :: Bool
  , isNamedPipe :: Bool
  , isRegularFile :: Bool
  , isDirectory :: Bool
  , isSymbolicLink :: Bool
  , isSocket :: Bool
  , permissions :: Permissions
} deriving (Show, Eq, Ord)




    ----------------------------
    --[ INSTANCES ]--
    ----------------------------


instance BF.Bifunctor File where
  bimap = BT.bimapDefault


instance BFL.Bifoldable File where
  bifoldMap = BT.bifoldMapDefault


instance BT.Bitraversable File where
    bitraverse f1 f2 (Dir n b) =
      Dir n <$> f1 b
    bitraverse _ f2 (RegFile n a) =
      RegFile n <$> f2 a
    bitraverse _ _ (Failed n e) =
      pure (Failed n e)



-- | First compare constructors: Failed < Dir < File...
-- Then compare `name`...
-- Then compare free variable parameter of `File` constructors
instance (Ord a, Ord b, Eq a, Eq b) => Ord (File a b) where
    compare (RegFile n a) (RegFile n' a') =
        case compare n n' of
             EQ -> compare a a'
             el -> el
    compare (Dir n b) (Dir n' b') =
        case compare n n' of
             EQ -> compare b b'
             el -> el
     -- after comparing above we can hand off to shape ord function:
    compare d d' = comparingShape d d'






    ----------------------------
    --[ HIGH LEVEL FUNCTIONS ]--
    ----------------------------


-- |Read a file into an `AnchoredFile`, filling the free variables via
-- `getFileInfo`. This also works on directories, but doesn't look at
-- their contents.
readFileWith :: (FilePath -> IO a)
             -> (FilePath -> IO b)
             -> FilePath
             -> IO (AnchoredFile a b)
readFileWith fd ff fp = do
  cfp <- canonicalizePath fp
  let fn = topDir cfp
      bd = baseDir cfp
  file <- handleDT fn $ do
    isFile <- doesFileExist cfp
    if isFile
       then RegFile fn <$> ff cfp
       else Dir fn <$> fd cfp
  return (bd :/ file)


readFile :: FilePath -> IO (AnchoredFile FileInfo FileInfo)
readFile fp = readFileWith getFileInfo getFileInfo =<< canonicalizePath fp


-- |Build a list of AnchoredFile, given the path to a directory, filling
-- the free variables via `getFileInfo`.
readDirectory :: FilePath -> IO [AnchoredFile FileInfo FileInfo]
readDirectory fp = readDirectoryWith getFileInfo getFileInfo
                     =<< canonicalizePath fp


-- | same as readDirectory but allows us to, for example, use
-- ByteString.readFile to return a tree of ByteStrings.
readDirectoryWith :: (FilePath -> IO a)
                  -> (FilePath -> IO b)
                  -> FilePath
                  -> IO [AnchoredFile a b]
readDirectoryWith fd ff p = buildWith' buildAtOnce' fd ff
                              =<< canonicalizePath p




    -----------------------------
    --[ LOWER LEVEL FUNCTIONS ]--
    -----------------------------




    -- -- -- helpers: -- -- --


type UserIO a = FilePath -> IO a
type Builder a b = UserIO a -> UserIO b -> FilePath -> IO [File a b]

-- remove non-existent file errors, which are artifacts of the "non-atomic"
-- nature of traversing a system firectory tree:
buildWith' :: Builder a b
           -> UserIO a
           -> UserIO b
           -> FilePath
           -> IO [AnchoredFile a b]
buildWith' bf' fd ff p =
  do
    cfp <- canonicalizePath p
    tree <- bf' fd ff cfp
    return $ fmap (cfp :/) (removeNonexistent tree)



-- IO function passed to our builder and finally executed here:
buildAtOnce' :: Builder a b
buildAtOnce' fd ff p = do
  cfp <- canonicalizePath p
  contents <- getAllDirsFiles cfp
  for contents $ \n -> handleDT n $ do
    let subf = cfp </> n
    do isFile <- doesFileExist subf
       if isFile
          then RegFile n <$> ff subf
          else Dir n <$> fd subf




    -----------------
    --[ UTILITIES ]--
    -----------------



---- HANDLING FAILURES ----


-- | True if any Failed constructors in the tree
anyFailed :: [File a b] -> Bool
anyFailed = not . successful

-- | True if there are no Failed constructors in the tree
successful :: [File a b] -> Bool
successful = null . failures


-- | returns true if argument is a `Failed` constructor:
failed :: File a b -> Bool
failed (Failed _ _) = True
failed _            = False


-- | returns a list of 'Failed' constructors only:
failures :: [File a b] -> [File a b]
failures = filter failed



---- ORDERING AND EQUALITY ----


-- | Tests equality of two trees, ignoring their free variable portion. Can be
-- used to check if any files have been added or deleted, for instance.
equalShape :: File a b -> File c d -> Bool
equalShape d d' = comparingShape d d' == EQ

-- TODO: we should use equalFilePath here, but how to sort properly?
-- with System.Directory.canonicalizePath, before compare?

-- | a compare function that ignores the free "file" type variable:
comparingShape :: File a b -> File c d -> Ordering
comparingShape (Dir n _) (Dir n' _) = compare n n'
-- else simply compare the flat constructors, non-recursively:
comparingShape t t'  = comparingConstr t t'


 -- HELPER: a non-recursive comparison
comparingConstr :: File a b -> File a1 b1 -> Ordering
comparingConstr (Failed _ _)  (Dir _ _)     = LT
comparingConstr (Failed _ _)  (RegFile _ _) = LT
comparingConstr (RegFile _ _) (Failed _ _)  = GT
comparingConstr (RegFile _ _) (Dir _ _)     = GT
comparingConstr (Dir _ _)     (Failed _ _)  = GT
comparingConstr (Dir _ _)     (RegFile _ _) = LT
 -- else compare on the names of constructors that are the same, without
 -- looking at the contents of Dir constructors:
comparingConstr t t'  = compare (name t) (name t')




---- OTHER ----



    ---------------
    --[ HELPERS ]--
    ---------------


---- CONSTRUCTOR IDENTIFIERS ----
isFileC :: File a b -> Bool
isFileC (RegFile _ _) = True
isFileC _             = False

isDirC :: File a b -> Bool
isDirC (Dir _ _) = True
isDirC _         = False


---- PATH CONVERSIONS ----



-- extracting pathnames and base names:
topDir, baseDir :: FilePath -> FilePath
topDir = last . splitDirectories
baseDir = joinPath . init . splitDirectories


hiddenFile :: FilePath -> Bool
hiddenFile "."  = False
hiddenFile ".." = False
hiddenFile str
  | "." `isPrefixOf` str = True
  | otherwise            = False


---- IO HELPERS: ----


-- |Go up one directory in the filesystem hierarchy.
goUp :: AnchoredFile FileInfo FileInfo -> IO (AnchoredFile FileInfo FileInfo)
goUp af@("" :/ _) = return af
goUp    (bp :/ _) = Data.DirTree.readFile bp


goUp' :: FilePath -> IO (AnchoredFile FileInfo FileInfo)
goUp' fp = do
  cfp <- canonicalizePath fp
  Data.DirTree.readFile $ baseDir cfp


getContents :: AnchoredFile FileInfo FileInfo
            -> IO [AnchoredFile FileInfo FileInfo]
getContents (bp :/ Dir n _) = readDirectory (bp </> n)
getContents _ = return []


-- |Get all files of a given directory and return them as a List.
-- This includes "." and "..".
getAllDirsFiles :: FilePath -> IO [FilePath]
getAllDirsFiles fp = do
  dirstream <- PFD.openDirStream fp
  let mdirs :: [FilePath] -> IO [FilePath]
      mdirs dirs = do
        dir <- PFD.readDirStream dirstream
        if dir == ""
          then return dirs
          else mdirs (dir : dirs)
  dirs <- mdirs []
  PFD.closeDirStream dirstream
  return dirs


-- |Get all files of a given directory and return them as a List.
-- This excludes "." and "..".
getDirsFiles :: FilePath -> IO [FilePath]
getDirsFiles fp = do
  dirstream <- PFD.openDirStream fp
  let mdirs :: [FilePath] -> IO [FilePath]
      mdirs dirs = do
        dir <- PFD.readDirStream dirstream
        if dir == ""
          then return dirs
          else mdirs (insert dir dirs)
  dirs <- mdirs []
  PFD.closeDirStream dirstream
  return dirs
  where
    insert dir dirs = case dir of
      "."  -> dirs
      ".." -> dirs
      _    -> dir : dirs


-- |Gets all file information.
getFileInfo :: FilePath -> IO FileInfo
getFileInfo fp = do
  fs <- PF.getSymbolicLinkStatus fp
  perms <- getPermissions fp
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
    (PF.isBlockDevice fs)
    (PF.isCharacterDevice fs)
    (PF.isNamedPipe fs)
    (PF.isRegularFile fs)
    (PF.isDirectory fs)
    (PF.isSymbolicLink fs)
    (PF.isSocket fs)
    perms


-- |Gets the free variable. Returns Nothing if the constructor is of `Failed`.
getFreeVar :: File a a -> Maybe a
getFreeVar (RegFile _ f)  = Just f
getFreeVar (Dir _  d)     = Just d
getFreeVar _              = Nothing


---- FAILURE HELPERS: ----


-- handles an IO exception by returning a Failed constructor filled with that
-- exception:
handleDT :: FileName -> IO (File a b) -> IO (File a b)
handleDT n = handle (return . Failed n)


-- DoesNotExist errors not present at the topmost level could happen if a
-- named file or directory is deleted after being listed by
-- getDirectoryContents but before we can get it into memory.
--    So we filter those errors out because the user should not see errors
-- raised by the internal implementation of this module:
--     This leaves the error if it exists in the top (user-supplied) level:
removeNonexistent :: [File a b] -> [File a b]
removeNonexistent = filter isOkConstructor
     where isOkConstructor c = not (failed c) || isOkError c
           isOkError = not . isDoesNotExistErrorType . ioeGetErrorType . err




---- OTHER: ----


fullPath :: AnchoredFile a b -> FilePath
fullPath (bp :/ f) = bp </> name f


fromFreeVar :: (Default d) => (a -> d) -> File a a -> d
fromFreeVar f df = maybeD f $ getFreeVar df


maybeD :: (Default b) => (a -> b) -> Maybe a -> b
maybeD = maybe def


-- |Pack the modification time
packModTime :: File FileInfo FileInfo
            -> String
packModTime = fromFreeVar
                $ show . posixSecondsToUTCTime . realToFrac . modificationTime

packPermissions :: File FileInfo FileInfo
                -> String
packPermissions dt = fromFreeVar (pStr . permissions) dt
  where
    pStr perm =    str perm readable   "r"
                ++ str perm writable   "w"
                ++ str perm (if isDirC dt then searchable else executable)
                            "x"
    str perm f ch
      | f perm    = ch
      | otherwise = "-"
