{-# OPTIONS_HADDOCK ignore-exports #-}

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
import Data.Ord
  (
    comparing
  )
import Data.List
  (
    delete
  , sort
  , sortBy
  , (\\)
  )
import Data.Time
  (
    UTCTime
  , formatTime
  )
import Data.Word
  (
    Word64
  )
import System.Directory
  (
    Permissions(..)
  , createDirectoryIfMissing
  , doesFileExist
  , getDirectoryContents
  , getModificationTime
  , getPermissions
  , writable
  , searchable
  )
import System.EasyFile
  (
    getCreationTime
  , getChangeTime
  , getAccessTime
  , getFileSize
  , hasSubDirectories
  , isSymlink
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
import qualified Data.Bitraversable as BT
import qualified Data.Bifunctor     as BF
import qualified Data.Bifoldable    as BFL
import qualified Data.Traversable   as T



    ----------------------------
    --[ BASE TYPES ]--
    ----------------------------


-- |Weak type to distinguish between FilePath and FileName.
type FileName = String


-- |A simple wrapper to hold a base directory name, which can be either an
-- absolute or relative path. This lets us give the DirTree a context, while
-- still letting us store only directory and file /names/ (not full paths) in
-- the DirTree. (uses an infix constructor; don't be scared)
data AnchoredDirTree a b =
  (:/) { anchor :: FilePath, dirTree :: DirTree a b }
  deriving (Eq, Ord, Show)


-- |The String in the name field is always a file name, never a full path.
-- The free type variable is used in the File/Dir constructor and can hold
-- Handles, Strings representing a file's contents or anything else you can
-- think of. We catch any IO errors in the Failed constructor. an Exception
-- can be converted to a String with 'show'.
data DirTree a b =
    Failed {
    name :: FileName
  , err  :: IOException
  }
  | Dir {
    name     :: FileName
  , contents :: [DirTree a b]
  , dir      :: a
  }
  | File {
    name :: FileName
  , file :: b
  } deriving Show


-- |All possible directory information we could ever need from a directory.
data DirTreeInfo =
  DirInfo {
    permissions  :: Permissions
  , creationTime :: Maybe UTCTime
  , changeTime   :: Maybe UTCTime
  , modTime      :: UTCTime
  , accessTime   :: UTCTime
  , sym          :: Bool
  , hasSubDirs   :: Maybe Bool
  }
  | FileInfo {
    permissions  :: Permissions
  , creationTime :: Maybe UTCTime
  , changeTime   :: Maybe UTCTime
  , modTime      :: UTCTime
  , accessTime   :: UTCTime
  , sym          :: Bool
  , fileSize     :: Word64
  }
  deriving (Show, Eq, Ord)




    ----------------------------
    --[ INSTANCES ]--
    ----------------------------


instance BF.Bifunctor DirTree where
  bimap = BT.bimapDefault


instance BFL.Bifoldable DirTree where
  bifoldMap = BT.bifoldMapDefault


instance BT.Bitraversable DirTree where
    bitraverse f1 f2 (Dir n cs b) =
      Dir n
        <$> T.traverse (BT.bitraverse f1 f2) cs
        <*> f1 b
    bitraverse _ f2 (File n a) =
      File n <$> f2 a
    bitraverse _ _ (Failed n e) =
      pure (Failed n e)


-- | Two DirTrees are equal if they have the same constructor, the same name
-- (and in the case of `Dir`s) their sorted `contents` are equal:
instance (Eq a, Eq b) => Eq (DirTree a b) where
    (File n a) == (File n' a') = n == n' && a == a'
    (Dir n cs _) == (Dir n' cs' _) =
        n == n' && sortBy comparingConstr cs == sortBy comparingConstr cs'
     -- after comparing above we can hand off to shape equality function:
    d == d' = equalShape d d'


-- | First compare constructors: Failed < Dir < File...
-- Then compare `name`...
-- Then compare free variable parameter of `File` constructors
instance (Ord a, Ord b, Eq a, Eq b) => Ord (DirTree a b) where
    compare (File n a) (File n' a') =
        case compare n n' of
             EQ -> compare a a'
             el -> el
    compare (Dir n cs b) (Dir n' cs' b') =
        case compare n n' of
             EQ -> case compare b b' of
               EQ -> comparing sort cs cs'
               el -> el
             el -> el
     -- after comparing above we can hand off to shape ord function:
    compare d d' = comparingShape d d'


-- for convenience:
instance BF.Bifunctor AnchoredDirTree where
    bimap fd ff (b:/d) = b :/ BF.bimap fd ff d



-- given the same fixity as <$>, is that right?
infixl 4 </$>



    ----------------------------
    --[ HIGH LEVEL FUNCTIONS ]--
    ----------------------------


-- | build an AnchoredDirTree, given the path to a directory, opening the files
-- using readFile.
-- Uses `readDirectoryWith` internally and has the effect of traversing the
-- entire directory structure. See `readDirectoryWithL` for lazy production
-- of a DirTree structure.
readDirectory :: FilePath -> IO (AnchoredDirTree String String)
readDirectory = readDirectoryWith readFile return


-- | same as readDirectory but allows us to, for example, use
-- ByteString.readFile to return a tree of ByteStrings.
readDirectoryWith :: (FilePath -> IO a)
                  -> (FilePath -> IO b)
                  -> FilePath
                  -> IO (AnchoredDirTree a b)
readDirectoryWith fd ff p = buildWith' buildAtOnce' fd ff p


-- | A "lazy" version of `readDirectoryWith` that does IO operations as needed
-- i.e. as the tree is traversed in pure code.
-- /NOTE:/ This function uses unsafeInterleaveIO under the hood. I believe
-- our use here is safe, but this function is experimental in this release:
readDirectoryWithL :: (FilePath -> IO a)
                   -> (FilePath -> IO b)
                   -> FilePath
                   -> IO (AnchoredDirTree a b)
readDirectoryWithL fd ff p = buildWith' buildLazilyUnsafe' fd ff p


-- | write a DirTree of strings to disk. Clobbers files of the same name.
-- Doesn't affect files in the directories (if any already exist) with
-- different names. Returns a new AnchoredDirTree where failures were
-- lifted into a `Failed` constructor:
writeDirectory :: AnchoredDirTree String String -> IO (AnchoredDirTree () ())
writeDirectory = writeDirectoryWith writeFile


-- | writes the directory structure to disk and uses the provided function to
-- write the contents of `Files` to disk. The return value of the function will
-- become the new `contents` of the returned, where IO errors at each node are
-- replaced with `Failed` constructors. The returned tree can be compared to
-- the passed tree to see what operations, if any, failed:
writeDirectoryWith :: (FilePath -> af -> IO bf)
                   -> AnchoredDirTree ad af
                   -> IO (AnchoredDirTree () bf)
writeDirectoryWith f (b:/t) = (b:/) <$> write' b t
    where write' b' (File n a) = handleDT n $
              File n <$> f (b'</>n) a
          write' b' (Dir n cs _) = handleDT n $
              do let bas = b'</>n
                 createDirectoryIfMissing True bas
                 Dir n <$> mapM (write' bas) cs <*> return ()
          write' _ (Failed n e) = return $ Failed n e






    -----------------------------
    --[ LOWER LEVEL FUNCTIONS ]--
    -----------------------------


-- | a simple application of readDirectoryWith openFile:
openDirectory :: FilePath -> IOMode -> IO (AnchoredDirTree () Handle)
openDirectory p m = readDirectoryWith (\_ -> return ()) (flip openFile m) p



-- | builds a DirTree from the contents of the directory passed to it, saving
-- the base directory in the Anchored* wrapper. Errors are caught in the tree in
-- the Failed constructor. The 'file' fields initially are populated with full
-- paths to the files they are abstracting.
build :: FilePath -> IO (AnchoredDirTree FilePath FilePath)
build = buildWith' buildAtOnce' return return   -- we say 'return' here to get
                                                -- back a  tree  of  FilePaths


-- | identical to `build` but does directory reading IO lazily as needed:
buildL :: FilePath -> IO (AnchoredDirTree FilePath FilePath)
buildL = buildWith' buildLazilyUnsafe' return return




    -- -- -- helpers: -- -- --


type UserIO a = FilePath -> IO a
type Builder a b = UserIO a -> UserIO b -> FilePath -> IO (DirTree a b)

-- remove non-existent file errors, which are artifacts of the "non-atomic"
-- nature of traversing a system firectory tree:
buildWith' :: Builder a b
           -> UserIO a
           -> UserIO b
           -> FilePath
           -> IO (AnchoredDirTree a b)
buildWith' bf' fd ff p =
    do tree <- bf' fd ff p
       return (baseDir p :/ removeNonexistent tree)



-- IO function passed to our builder and finally executed here:
buildAtOnce' :: Builder a b
buildAtOnce' fd ff p = handleDT n $
           do isFile <- doesFileExist p
              if isFile
                 then  File n <$> ff p
                 else do cs <- getDirsFiles p
                         Dir n
                           <$> T.mapM (buildAtOnce' fd ff . combine p) cs
                           <*> fd p
     where n = topDir p


-- using unsafeInterleaveIO to get "lazy" traversal:
buildLazilyUnsafe' :: Builder a b
buildLazilyUnsafe' fd ff p = handleDT n $
           do isFile <- doesFileExist p
              if isFile
                 then File n <$> ff p
                 -- HERE IS THE UNSAFE CODE:
                 else do
                   -- this is not behind unsafeInterleaveIO on purpose
                   -- otherwise we might get runtime exceptions
                   files <- getDirsFiles p
                   contents <- unsafeInterleaveIO $
                      mapM (rec . combine p) files
                   d <- fd p
                   return $ Dir n contents d
     where rec = buildLazilyUnsafe' fd ff
           n = topDir p




    -----------------
    --[ UTILITIES ]--
    -----------------



---- HANDLING FAILURES ----


-- | True if any Failed constructors in the tree
anyFailed :: DirTree a b -> Bool
anyFailed = not . successful

-- | True if there are no Failed constructors in the tree
successful :: DirTree a b -> Bool
successful = null . failures


-- | returns true if argument is a `Failed` constructor:
failed :: DirTree a b -> Bool
failed (Failed _ _) = True
failed _            = False


-- | returns a list of 'Failed' constructors only:
failures :: DirTree a b -> [DirTree a b]
failures = filter failed . flattenDir


-- | maps a function to convert Failed DirTrees to Files or Dirs
failedMap :: (FileName -> IOException -> DirTree a b) -> DirTree a b -> DirTree a b
failedMap f = transformDir unFail
    where unFail (Failed n e) = f n e
          unFail c            = c


---- ORDERING AND EQUALITY ----


-- | Recursively sort a directory tree according to the Ord instance
sortDir :: (Ord a, Ord b) => DirTree a b -> DirTree a b
sortDir = sortDirBy compare

-- | Recursively sort a tree as in `sortDir` but ignore the file contents of a
-- File constructor
sortDirShape :: DirTree a b -> DirTree a b
sortDirShape = sortDirBy comparingShape  where

  -- HELPER:
sortDirBy :: (DirTree a b -> DirTree a b -> Ordering) -> DirTree a b -> DirTree a b
sortDirBy cf = transformDir sortD
    where sortD (Dir n cs a) = Dir n (sortBy cf cs) a
          sortD c            = c


-- | Tests equality of two trees, ignoring their free variable portion. Can be
-- used to check if any files have been added or deleted, for instance.
equalShape :: DirTree a b -> DirTree c d -> Bool
equalShape d d' = comparingShape d d' == EQ

-- TODO: we should use equalFilePath here, but how to sort properly? with System.Directory.canonicalizePath, before compare?

-- | a compare function that ignores the free "file" type variable:
comparingShape :: DirTree a b -> DirTree c d -> Ordering
comparingShape (Dir n cs _) (Dir n' cs' _) =
    case compare n n' of
         EQ -> comp (sortCs cs) (sortCs cs')
         el -> el
    where sortCs = sortBy comparingConstr
           -- stolen from [] Ord instance:
          comp []     []     = EQ
          comp []     (_:_)  = LT
          comp (_:_)  []     = GT
          comp (x:xs) (y:ys) = case comparingShape x y of
                                    EQ    -> comp xs ys
                                    other -> other
 -- else simply compare the flat constructors, non-recursively:
comparingShape t t'  = comparingConstr t t'


 -- HELPER: a non-recursive comparison
comparingConstr :: DirTree a b -> DirTree a1 b1 -> Ordering
comparingConstr (Failed _ _) (Dir _ _ _)  = LT
comparingConstr (Failed _ _) (File _ _)   = LT
comparingConstr (File _ _)   (Failed _ _) = GT
comparingConstr (File _ _)   (Dir _ _ _)  = GT
comparingConstr (Dir _ _ _)  (Failed _ _) = GT
comparingConstr (Dir _ _ _)  (File _ _)   = LT
 -- else compare on the names of constructors that are the same, without
 -- looking at the contents of Dir constructors:
comparingConstr t t'  = compare (name t) (name t')




---- OTHER ----

-- | If the argument is a 'Dir' containing a sub-DirTree matching 'FileName'
-- then return that subtree, appending the 'name' of the old root 'Dir' to the
-- 'anchor' of the AnchoredDirTree wrapper. Otherwise return @Nothing@.
dropTo :: FileName -> AnchoredDirTree a b -> Maybe (AnchoredDirTree a b)
dropTo n' (p :/ Dir n ds' _) = search ds'
    where search [] = Nothing
          search (d:ds) | equalFilePath n' (name d) = Just ((p</>n) :/ d)
                        | otherwise = search ds
dropTo _ _ = Nothing


find :: FilePath
     -> AnchoredDirTree a b
     -> Either String (AnchoredDirTree a b)
find f d = findAbs f d <|> findRel f d


-- |Finds a file or directory inside an @AnchoredDirTree@. This only
-- looks at the subdirectories of the underlying @DirTree@. If you
-- want to compare the name of the topmost @DirTree@ as well, use @find'@.
findRel :: FilePath
        -> AnchoredDirTree a b
        -> Either String (AnchoredDirTree a b)
findRel f d =
  go (splitDirectories f) d
  where
    go (f:fs) (p :/ Dir n ds _) = search ds f >>= go fs
      where
        search [] _ = Left "Directory or file not found!"
        search (d:ds) n | equalFilePath n (name d) = Right ((p</>n) :/ d)
                        | otherwise                = search ds n
    go [] d = Right d
    go _ (p :/ Failed _ err) = Left $ show err
    go _ _  = Left "Directory or file not found!"


-- |Finds a file or directory inside an @AnchoredDirTree@. This also
-- looks at the topmost @DirTree@ and compares the first path component
-- with it. If you only want to look at subdirectories, use @find@.
findAbs :: FilePath
        -> AnchoredDirTree a b
        -> Either String (AnchoredDirTree a b)
findAbs f d =
  go (splitDirectories f) d
  where
    go (f':fs) (_ :/ Dir n _ _)
      | equalFilePath f' n = find (joinPath fs) d
      | otherwise          = Left "Directory or file not found!"
    go _ (p :/ Failed _ err) = Left $ show err
    go _ _  = Left "Directory or file not found!"


-- | applies the predicate to each constructor in the tree, removing it (and
-- its children, of course) when the predicate returns False. The topmost
-- constructor will always be preserved:
filterDir :: (DirTree a b -> Bool) -> DirTree a b -> DirTree a b
filterDir p = transformDir filterD
    where filterD (Dir n cs a) = Dir n (filter p cs) a
          filterD c            = c


-- | Flattens a `DirTree` into a (never empty) list of tree constructors. `Dir`
-- constructors will have [] as their `contents`:
flattenDir :: DirTree a b -> [ DirTree a b ]
flattenDir (Dir n cs a) = Dir n [] a : concatMap flattenDir cs
flattenDir f            = [f]





-- | Allows for a function on a bare DirTree to be applied to an AnchoredDirTree
-- within a Functor. Very similar to and useful in combination with `<$>`:
(</$>) :: (Functor f) => (DirTree a a1 -> DirTree b b1) -> f (AnchoredDirTree a a1) ->
                         f (AnchoredDirTree b b1)
(</$>) f = fmap (\(b :/ t) -> b :/ f t)


    ---------------
    --[ HELPERS ]--
    ---------------


---- CONSTRUCTOR IDENTIFIERS ----
isFileC :: DirTree a b -> Bool
isFileC (File _ _) = True
isFileC _          = False

isDirC :: DirTree a b -> Bool
isDirC (Dir _ _ _) = True
isDirC _           = False


---- PATH CONVERSIONS ----



-- | tuple up the complete file path with the 'file' contents, by building up the
-- path, trie-style, from the root. The filepath will be relative to \"anchored\"
-- directory.
--
-- This allows us to, for example, @mapM_ uncurry writeFile@ over a DirTree of
-- strings, although 'writeDirectory' does a better job of this.
zipPaths :: AnchoredDirTree a b -> DirTree (FilePath, a) (FilePath, b)
zipPaths (b :/ t) = zipP b t
    where zipP p (File n a)   = File n (p</>n , a)
          zipP p (Dir n cs a) = Dir n (map (zipP $ p</>n) cs) (p</>n , a)
          zipP _ (Failed n e) = Failed n e


-- extracting pathnames and base names:
topDir, baseDir :: FilePath -> FilePath
topDir = last . splitDirectories
baseDir = joinPath . init . splitDirectories



---- IO HELPERS: ----


-- | writes the directory structure (not files) of a DirTree to the anchored
-- directory. Returns a structure identical to the supplied tree with errors
-- replaced by `Failed` constructors:
writeJustDirs :: AnchoredDirTree a b -> IO (AnchoredDirTree () b)
writeJustDirs = writeDirectoryWith (const return)


----- the let expression is an annoying hack, because dropFileName "." == ""
----- and getDirectoryContents fails epically on ""
-- prepares the directory contents list. we sort so that we can be sure of
-- a consistent fold/traversal order on the same directory:
getDirsFiles :: String -> IO [FilePath]
getDirsFiles cs = do let cs' = if null cs then "." else cs
                     dfs <- getDirectoryContents cs'
                     return $ dfs \\ [".",".."]


readPath :: FilePath
         -> IO (AnchoredDirTree DirTreeInfo DirTreeInfo)
readPath = readDirectoryWithL mkDirInfo mkFileInfo


mkFileInfo :: FilePath -> IO DirTreeInfo
mkFileInfo fp =
  FileInfo
    <$> getPermissions      fp
    <*> getCreationTime     fp
    <*> getChangeTime       fp
    <*> getModificationTime fp
    <*> getAccessTime       fp
    <*> isSymlink           fp
    <*> getFileSize         fp


mkDirInfo :: FilePath -> IO DirTreeInfo
mkDirInfo fp =
  DirInfo
    <$> getPermissions      fp
    <*> getCreationTime     fp
    <*> getChangeTime       fp
    <*> getModificationTime fp
    <*> getAccessTime       fp
    <*> isSymlink           fp
    <*> hasSubDirectories   fp


getFreeVar :: DirTree a a -> Maybe a
getFreeVar (File _ f)  = Just f
getFreeVar (Dir _ _ d) = Just d
getFreeVar _           = Nothing


---- FAILURE HELPERS: ----


-- handles an IO exception by returning a Failed constructor filled with that
-- exception:
handleDT :: FileName -> IO (DirTree a b) -> IO (DirTree a b)
handleDT n = handle (return . Failed n)


-- DoesNotExist errors not present at the topmost level could happen if a
-- named file or directory is deleted after being listed by
-- getDirectoryContents but before we can get it into memory.
--    So we filter those errors out because the user should not see errors
-- raised by the internal implementation of this module:
--     This leaves the error if it exists in the top (user-supplied) level:
removeNonexistent :: DirTree a b -> DirTree a b
removeNonexistent = filterDir isOkConstructor
     where isOkConstructor c = not (failed c) || isOkError c
           isOkError = not . isDoesNotExistErrorType . ioeGetErrorType . err


-- | At 'Dir' constructor, apply transformation function to all of directory's
-- contents, then remove the Nothing's and recurse. This always preserves the
-- topomst constructor.
transformDir :: (DirTree a b -> DirTree a b) -> DirTree a b -> DirTree a b
transformDir f t = case f t of
                     (Dir n cs a) -> Dir n (map (transformDir f) cs) a
                     t'           -> t'



---- OTHER: ----


anchoredToPath :: AnchoredDirTree a b -> FilePath
anchoredToPath a = anchor a </> (name . dirTree $ a)


ls :: DirTree DirTreeInfo DirTreeInfo
   -> [(FileName, String)]
ls dt = fmap (\x -> (name x, packModTime x)) (contents dt)


fromFreeVar :: (Default d) => (a -> d) -> DirTree a a -> d
fromFreeVar f dt = maybeD f $ getFreeVar dt


maybeD :: (Default b) => (a -> b) -> Maybe a -> b
maybeD = maybe def


-- |Pack the modification time
packModTime :: DirTree DirTreeInfo DirTreeInfo
            -> String
packModTime = fromFreeVar
                $ formatTime defaultTimeLocale rfc822DateFormat
                . modTime


packPermissions :: DirTree DirTreeInfo DirTreeInfo
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
