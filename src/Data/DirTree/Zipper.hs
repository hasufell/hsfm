{-# OPTIONS_HADDOCK ignore-exports #-}

module Data.DirTree.Zipper where


import Control.Applicative
  (
    (<$>)
  )
import Control.Arrow
  (
    first
  )
import Data.DirTree
import System.Directory
  (
    canonicalizePath
  )
import System.FilePath
 (
    equalFilePath
  , splitPath
  , takeDirectory
  , (</>)
 )
import System.IO.Unsafe
  (
    unsafeInterleaveIO
  )
import qualified Data.List as DL


    ----------------------------
    --[ ZIPPING ]--
    ----------------------------


-- |The zipper type, left is the (current) directory, right
-- are the breadcrumbs.
type DTZipper a b = (DirTree a b, [DirTree a b])

type DTInfoZipper = DTZipper DirTreeInfo DirTreeInfo


-- |The base zipper of a tree with empty crumbs element.
baseZipper :: DirTree a b -> DTZipper a b
baseZipper dt = (dt, [])


-- |Goes down the given subdir or file in a given directory. Returns `Nothing`
-- if the subdir or file does not exist.
--
-- Note that this function can be slow, so it's not supposed to be called
-- over a list of zippers. Use `goAllDown` instead.
goDown :: FileName -> DTZipper a b -> Maybe (DTZipper a b)
goDown fn (dtp@(Dir n cs d), xs) =
  case mcdt of
    Just cdt -> Just (cdt, Dir n (crumb' fn cs) d : xs)
    Nothing  -> Nothing
  where
    mcdt  = DL.find (\x -> equalFilePath (name x) fn) cs
goDown _ _            = Nothing


-- |Goes down all subdirs of a given directory.
goAllDown :: DTZipper a b -> [DTZipper a b]
goAllDown (Dir n cs d, xs) = fmap (\x -> (x, Dir n (crumb x cs) d : xs)) cs
goAllDown _                = []


-- |Goes down the given subpath in a given directory. Returns `Nothing`
-- if the subpath does not exist.
goDown' :: FilePath -> DTZipper a b -> Maybe (DTZipper a b)
goDown' fp dz = go (splitPath fp) dz
  where
    go []  dz      = Just dz
    go (fn:fns) dz = goDown fn dz >>= go fns


-- TODO: error handling if the parent of a file is a file too (wat?)
-- |Goes up one directory. This cannot fail. If you call it on the
-- root node of the zipper, you get it back untouched.
goUp :: DTZipper a b -> DTZipper a b
goUp dz@(_, [])            = dz
goUp (dt, Dir n cs d : xs) = (Dir n (dt:cs) d, xs)


-- |Goes up to the root directory/node of the zipper.
goRoot :: DTZipper a b -> DTZipper a b
goRoot dz@(_, []) = dz
goRoot dz         = goRoot (goUp dz)


-- |Gets the full path of the current directory in the zipper context.
-- This might not be a real absolute filesystem path, because it depends
-- on the zipper context.
getFullPath :: DTZipper a b -> FilePath
getFullPath dz@(dt, _:_) = getFullPath (goUp dz) </> name dt
getFullPath    (dt, [])  = name dt


-- |Retrieve the (current) directory component from the zipper.
unZip :: DTZipper a b -> DirTree a b
unZip = fst


-- |Retrieve the (current) directory component from the zipper and
-- transform it to an `AnchoredDirTree`.
unZip' :: DTZipper a b -> AnchoredDirTree a b
unZip' dz@(dt, _) = (takeDirectory . getFullPath $ dz) :/ dt


-- |Map a function over the (current) directory component of the zipper.
zipMap :: (DirTree a b -> DirTree a b) -> DTZipper a b -> DTZipper a b
zipMap = first


-- |Creates a zipper at the given location with lazy breadcrumbs. That
-- means it doesn't traverse to the destination directory through the whole
-- tree.
--
-- This can throw an exception on `canonicalizePath`.
--
-- It uses `unsafeInterleaveIO` and `readDirectoryWithL` to achieve
-- lazy traversal.
zipLazy :: (FilePath -> IO a)  -- ^ builder function for the free dir var
        -> (FilePath -> IO b)  -- ^ builder function for the free file var
        -> FilePath            -- ^ file path to drop to
        -> IO (DTZipper a b)
zipLazy fd ff fp = do
  dt <- dirTree <$> readDirectoryWithL fd ff fp
  go dt fp
  where
    go dt fp' = do
      -- TODO: I hope parentDir doesn't blow up
      parentDir <- canonicalizePath (fp' ++ "/..")
      if fp' == parentDir
         then return $ baseZipper dt
         else do
           -- HERE IS THE UNSAFE CODE:
           crumbs <- unsafeInterleaveIO $ crumbrec parentDir
           return (dt, crumbs)
      where
        crumbrec pD = do
          pdt@(Dir n cs d) <- dirTree <$> readDirectoryWithL fd ff pD
          (_, pc) <- go pdt pD
          return $ Dir n (crumb dt cs) d : pc


readPath' :: FilePath -> IO (DTZipper DirTreeInfo DirTreeInfo)
readPath' = zipLazy mkDirInfo mkFileInfo


    ---------------
    --[ HELPERS ]--
    ---------------


crumb :: DirTree a b -> [DirTree a b] -> [DirTree a b]
crumb dt cs = crumb' (name dt) cs


crumb' :: FileName -> [DirTree a b] -> [DirTree a b]
crumb' fn cs =
  foldr (\x y -> if equalFilePath fn (name x) then y else x : y)
        [] cs
