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
import Data.IntMap.Lazy (IntMap)
import Data.List
  (
    delete
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
import System.Directory
  (
    doesFileExist
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
import qualified Data.IntMap.Lazy as IM




    ----------------------------
    --[ BASE TYPES ]--
    ----------------------------


-- |Weak type to distinguish between FilePath and FileName.
type FileName = String


-- |Represents a directory with it's contents (one level only), while
-- preserving the context via the anchor.
data AnchoredDirFile a b =
  (:/) { anchor :: FilePath, dirTree :: IntMap (DirFile a b) }
  deriving (Eq, Ord, Show)


-- |The String in the name field is always a file name, never a full path.
-- The free type variable is used in the File/Dir constructor and can hold
-- Handles, Strings representing a file's contents or anything else you can
-- think of. We catch any IO errors in the Failed constructor. an Exception
-- can be converted to a String with 'show'.
data DirFile a b =
    Failed {
    name :: FileName
  , err  :: IOException
  }
  | Dir {
    name :: FileName
  , dir  :: a
  }
  | File {
    name :: FileName
  , file :: b
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


instance BF.Bifunctor DirFile where
  bimap = BT.bimapDefault


instance BFL.Bifoldable DirFile where
  bifoldMap = BT.bifoldMapDefault


instance BT.Bitraversable DirFile where
    bitraverse f1 f2 (Dir n b) =
      Dir n <$> f1 b
    bitraverse _ f2 (File n a) =
      File n <$> f2 a
    bitraverse _ _ (Failed n e) =
      pure (Failed n e)



-- | First compare constructors: Failed < Dir < File...
-- Then compare `name`...
-- Then compare free variable parameter of `File` constructors
instance (Ord a, Ord b, Eq a, Eq b) => Ord (DirFile a b) where
    compare (File n a) (File n' a') =
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


-- | build an AnchoredDirFile, given the path to a directory, opening the files
-- using readFile.
-- Uses `readDirectoryWith` internally and has the effect of traversing the
-- entire directory structure. See `readDirectoryWithL` for lazy production
-- of a DirFile structure.
readDirectory :: FilePath -> IO (AnchoredDirFile String String)
readDirectory = readDirectoryWith readFile return


-- | same as readDirectory but allows us to, for example, use
-- ByteString.readFile to return a tree of ByteStrings.
readDirectoryWith :: (FilePath -> IO a)
                  -> (FilePath -> IO b)
                  -> FilePath
                  -> IO (AnchoredDirFile a b)
readDirectoryWith fd ff p = buildWith' buildAtOnce' fd ff p




    -----------------------------
    --[ LOWER LEVEL FUNCTIONS ]--
    -----------------------------



-- | builds a DirFile from the contents of the directory passed to it, saving
-- the base directory in the Anchored* wrapper. Errors are caught in the tree in
-- the Failed constructor. The 'file' fields initially are populated with full
-- paths to the files they are abstracting.
build :: FilePath -> IO (AnchoredDirFile FilePath FilePath)
build = buildWith' buildAtOnce' return return   -- we say 'return' here to get
                                                -- back a  tree  of  FilePaths



    -- -- -- helpers: -- -- --


type UserIO a = FilePath -> IO a
type Builder a b = UserIO a -> UserIO b -> FilePath -> IO (IntMap (DirFile a b))

-- remove non-existent file errors, which are artifacts of the "non-atomic"
-- nature of traversing a system firectory tree:
buildWith' :: Builder a b
           -> UserIO a
           -> UserIO b
           -> FilePath
           -> IO (AnchoredDirFile a b)
buildWith' bf' fd ff p =
    do tree <- bf' fd ff p
       return (p :/ removeNonexistent tree)



-- IO function passed to our builder and finally executed here:
buildAtOnce' :: Builder a b
buildAtOnce' fd ff p = do
  contents <- getDirsFiles p
  for contents $ \n -> handleDT n $ do
    let subf = p </> n
    do isFile <- doesFileExist subf
       if isFile
          then File n <$> ff subf
          else Dir n <$> fd subf




    -----------------
    --[ UTILITIES ]--
    -----------------



---- HANDLING FAILURES ----


-- | True if any Failed constructors in the tree
anyFailed :: [DirFile a b] -> Bool
anyFailed = not . successful

-- | True if there are no Failed constructors in the tree
successful :: [DirFile a b] -> Bool
successful = null . failures


-- | returns true if argument is a `Failed` constructor:
failed :: DirFile a b -> Bool
failed (Failed _ _) = True
failed _            = False


-- | returns a list of 'Failed' constructors only:
failures :: [DirFile a b] -> [DirFile a b]
failures = filter failed



---- ORDERING AND EQUALITY ----


-- | Tests equality of two trees, ignoring their free variable portion. Can be
-- used to check if any files have been added or deleted, for instance.
equalShape :: DirFile a b -> DirFile c d -> Bool
equalShape d d' = comparingShape d d' == EQ

-- TODO: we should use equalFilePath here, but how to sort properly?
-- with System.Directory.canonicalizePath, before compare?

-- | a compare function that ignores the free "file" type variable:
comparingShape :: DirFile a b -> DirFile c d -> Ordering
comparingShape (Dir n _) (Dir n' _) = compare n n'
-- else simply compare the flat constructors, non-recursively:
comparingShape t t'  = comparingConstr t t'


 -- HELPER: a non-recursive comparison
comparingConstr :: DirFile a b -> DirFile a1 b1 -> Ordering
comparingConstr (Failed _ _) (Dir _ _)    = LT
comparingConstr (Failed _ _) (File _ _)   = LT
comparingConstr (File _ _)   (Failed _ _) = GT
comparingConstr (File _ _)   (Dir _ _)    = GT
comparingConstr (Dir _ _)    (Failed _ _) = GT
comparingConstr (Dir _ _)    (File _ _)   = LT
 -- else compare on the names of constructors that are the same, without
 -- looking at the contents of Dir constructors:
comparingConstr t t'  = compare (name t) (name t')




---- OTHER ----



    ---------------
    --[ HELPERS ]--
    ---------------


---- CONSTRUCTOR IDENTIFIERS ----
isFileC :: DirFile a b -> Bool
isFileC (File _ _) = True
isFileC _          = False

isDirC :: DirFile a b -> Bool
isDirC (Dir _ _) = True
isDirC _         = False


---- PATH CONVERSIONS ----



-- extracting pathnames and base names:
topDir, baseDir :: FilePath -> FilePath
topDir = last . splitDirectories
baseDir = joinPath . init . splitDirectories



---- IO HELPERS: ----



----- the let expression is an annoying hack, because dropFileName "." == ""
----- and getDirectoryContents fails epically on ""
-- prepares the directory contents list. we sort so that we can be sure of
-- a consistent fold/traversal order on the same directory:
getDirsFiles :: FilePath -> IO (IntMap FilePath)
getDirsFiles fp = do
  dirstream <- PFD.openDirStream fp
  let mdirs :: Int -> IntMap FilePath -> IO (IntMap FilePath)
      mdirs ix dirs = do
        dir <- PFD.readDirStream dirstream
        if dir == ""
          then return dirs
          else mdirs (ix + 1) (instertF ix dir dirs)
  dirs <- mdirs 0 IM.empty
  PFD.closeDirStream dirstream
  return dirs
  where
    instertF ix dir dirs = case dir of
      "."  -> dirs
      ".." -> dirs
      _    -> IM.insert ix dir dirs


-- |Read a filepath and return transform it into our `AnchoredDirFile`
-- with the free variables of both the File and Dir constructors filled
-- with `FileInfo`.
readPath :: FilePath
         -> IO (AnchoredDirFile FileInfo FileInfo)
readPath = readDirectoryWith getFileInfo getFileInfo


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
getFreeVar :: DirFile a a -> Maybe a
getFreeVar (File _ f)  = Just f
getFreeVar (Dir _  d)  = Just d
getFreeVar _           = Nothing


---- FAILURE HELPERS: ----


-- handles an IO exception by returning a Failed constructor filled with that
-- exception:
handleDT :: FileName -> IO (DirFile a b) -> IO (DirFile a b)
handleDT n = handle (return . Failed n)


-- DoesNotExist errors not present at the topmost level could happen if a
-- named file or directory is deleted after being listed by
-- getDirectoryContents but before we can get it into memory.
--    So we filter those errors out because the user should not see errors
-- raised by the internal implementation of this module:
--     This leaves the error if it exists in the top (user-supplied) level:
removeNonexistent :: IntMap (DirFile a b) -> IntMap (DirFile a b)
removeNonexistent = IM.filter isOkConstructor
     where isOkConstructor c = not (failed c) || isOkError c
           isOkError = not . isDoesNotExistErrorType . ioeGetErrorType . err




---- OTHER: ----


-- TODO: use Maybe type?
dirLookup :: AnchoredDirFile a b -> Int -> DirFile a b
dirLookup df ix =
  fromMaybe errTree (IM.lookup ix . dirTree $ df)
  where
    errTree = Failed "Not found!!!"
                     (userError $ "Failed to lookup index " ++ show ix)


subDirName :: AnchoredDirFile a b -> Int -> FilePath
subDirName df ix = anchor df </> name (dirLookup df ix)


fromFreeVar :: (Default d) => (a -> d) -> DirFile a a -> d
fromFreeVar f df = maybeD f $ getFreeVar df


maybeD :: (Default b) => (a -> b) -> Maybe a -> b
maybeD = maybe def


-- |Pack the modification time
packModTime :: DirFile FileInfo FileInfo
            -> String
packModTime = fromFreeVar
                $ show . posixSecondsToUTCTime . realToFrac . modificationTime

packPermissions :: DirFile FileInfo FileInfo
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
