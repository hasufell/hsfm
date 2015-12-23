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
  , normalise
  , equalFilePath
  , isAbsolute
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
data AnchoredFile a =
  (:/) { anchor :: FilePath, file :: File a }
  deriving (Eq, Ord, Show)


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


symlOrRegFile :: AnchoredFile FileInfo
              -> (Bool, AnchoredFile FileInfo)
symlOrRegFile f@(_ :/ RegFile {}) = (True, f)
symlOrRegFile f@(_ :/ SymLink {}) = (True, f)
symlOrRegFile f = (False, f)


sdir :: AnchoredFile FileInfo -> (Bool, AnchoredFile FileInfo)
sdir f@(_ :/ SymLink { sdest = (_ :/ Dir {} )}) = (True, f)
sdir f@(_ :/ Dir {})                     = (True, f)
sdir f = (False, f)


pattern SymlOrRegFile <- (symlOrRegFile -> (True, _))

pattern SDir f <- (sdir -> (True, f))



    ----------------------------
    --[ INSTANCES ]--
    ----------------------------


-- | First compare constructors: Failed < Dir < File...
-- Then compare `name`...
-- Then compare free variable parameter of `File` constructors
instance (Ord a, Eq a) => Ord (File a) where
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
readFile fp = readFileWith getFileInfo =<< canonicalizePath' fp


-- |Build a list of AnchoredFile, given the path to a directory, filling
-- the free variables via `getFileInfo`. This includes the "." and ".."
-- directories.
readDirectory :: FilePath -> IO [AnchoredFile FileInfo]
readDirectory fp = readDirectoryWith getAllDirsFiles getFileInfo
                     =<< canonicalizePath' fp


-- |Build a list of AnchoredFile, given the path to a directory, filling
-- the free variables via `getFileInfo`. This excludes the "." and ".."
-- directories.
readDirectory' :: FilePath -> IO [AnchoredFile FileInfo]
readDirectory' fp = readDirectoryWith getDirsFiles getFileInfo
                      =<< canonicalizePath' fp


-- | same as readDirectory but allows us to, for example, use
-- ByteString.readFile to return a tree of ByteStrings.
readDirectoryWith :: (FilePath -> IO [FilePath])
                  -> (FilePath -> IO a)
                  -> FilePath
                  -> IO [AnchoredFile a]
readDirectoryWith getfiles ff p = do
  contents <- getfiles =<< canonicalizePath' p
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


-- | Tests equality of two trees, ignoring their free variable portion. Can be
-- used to check if any files have been added or deleted, for instance.
equalShape :: File a -> File b -> Bool
equalShape d d' = comparingShape d d' == EQ

-- TODO: we should use equalFilePath here, but how to sort properly?
-- with System.Directory.canonicalizePath, before compare?

-- | a compare function that ignores the free "file" type variable:
comparingShape :: File a -> File b -> Ordering
comparingShape (Dir n _) (Dir n' _) = compare n n'
-- else simply compare the flat constructors, non-recursively:
comparingShape t t'  = comparingConstr t t'


 -- HELPER: a non-recursive comparison
comparingConstr :: File a -> File b -> Ordering
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
isFileC :: File a -> Bool
isFileC (RegFile _ _) = True
isFileC _             = False

isDirC :: File a -> Bool
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


-- |Like `canonicalizePath` from System.Directory, but preserves the last
-- component if it's a symlink.
canonicalizePath' :: FilePath -> IO FilePath
canonicalizePath' fp = do
  -- TODO: throw fileDoesNotExist error earlier
  isSymlink <- PF.isSymbolicLink <$> PF.getSymbolicLinkStatus fp
  if isSymlink
    then do
         cbase <- canonicalizePath (baseDir fp)
         return $ cbase </> topDir fp
    else canonicalizePath fp


---- IO HELPERS: ----


-- |Go up one directory in the filesystem hierarchy.
goUp :: AnchoredFile FileInfo -> IO (AnchoredFile FileInfo)
goUp af@("" :/ _) = return af
goUp    (bp :/ _) = Data.DirTree.readFile bp


goUp' :: FilePath -> IO (AnchoredFile FileInfo)
goUp' fp = do
  cfp <- canonicalizePath' fp
  Data.DirTree.readFile $ baseDir cfp


getContents :: AnchoredFile FileInfo
            -> IO [AnchoredFile FileInfo]
getContents (SDir af) = readDirectory (fullPath af)
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
getFreeVar (RegFile _ f)  = Just f
getFreeVar (Dir _  d)     = Just d
getFreeVar _              = Nothing


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




---- OTHER: ----


fullPath :: AnchoredFile a -> FilePath
fullPath (bp :/ f) = bp </> name f


fromFreeVar :: (Default d) => (a -> d) -> File a -> d
fromFreeVar f df = maybeD f $ getFreeVar df


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
