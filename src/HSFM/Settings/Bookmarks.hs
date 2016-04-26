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

module HSFM.Settings.Bookmarks where


import Control.Monad
  (
    void
  )
import Data.Attoparsec.ByteString
import qualified Data.ByteString as BS
import Data.ByteString
  (
    ByteString
  )
import Data.Maybe
  (
    catMaybes
  , fromJust
  )
import Data.Word8
  (
    _nul
  )
import qualified HPath as P
import HPath
  (
    Abs
  , Fn
  , Path
  )
import HSFM.FileSystem.FileOperations
import HSFM.FileSystem.FileType
import Prelude hiding (readFile, writeFile)
import System.Posix.Env.ByteString
  (
    getEnv
  )



-- |A bookmark. `bkName` is principally a description of the bookmark
-- but must satisfy the constraints of a filename.
data Bookmark = MkBookmark {
    bkName :: Path Fn
  , bkPath :: Path Abs
} deriving (Show)


-- |Parses bookmarks from a ByteString that has pairs of
-- name and path. Name and path are separated by one null character
-- and the pairs itself are separated by two null characters from
-- each other.
bkParser :: Parser [Bookmark]
bkParser =
  fmap catMaybes $ many' (fmap toBm $ bookmark <* word8 _nul <* word8 _nul)
  where
    toBm :: (ByteString, ByteString) -> Maybe Bookmark
    toBm (name, path) = MkBookmark <$> P.parseFn name
                                   <*> P.parseAbs path
    bookmark :: Parser (ByteString, ByteString)
    bookmark =
      (\x y -> (BS.pack x, BS.pack y))
      <$> many1' char
      <* (word8 _nul)
      <*> many1' char
    char = satisfy (`notElem` [_nul])


-- |Writes bookmarks to a given file.
writeBookmarks :: [Bookmark] -> IO ()
writeBookmarks [] = return ()
writeBookmarks bs = do
  bf  <- bookmarksFile
  bfd <- bookmarksDir
  mkdirP bfd
  readFile getFileInfo bfd >>= (\x -> createFile x bookmarksFileName)
  let str = foldr1 (\x y -> x `BS.append` BS.pack [_nul, _nul]
                              `BS.append`
                            y `BS.append` BS.pack [_nul, _nul])
            (fmap toByteString bs)
  file <- readFile getFileInfo bf
  void $ writeFile file str
    where
    toByteString :: Bookmark -> ByteString
    toByteString b =             (P.fromRel $ bkName b)
                     `BS.append` BS.singleton _nul
                     `BS.append` (P.fromAbs $ bkPath b)


-- |Reads bookmarks from a given file.
readBookmarks :: IO [Bookmark]
readBookmarks = do
  p    <- bookmarksFile
  file <- readFile getFileInfo p
  c    <- readFileContents file
  case parseOnly bkParser c of
    Left _  -> return []
    Right x -> return x


bookmarksDir :: IO (Path Abs)
bookmarksDir = do
  mhomedir  <- getEnv "HOME"
  case mhomedir of
    Nothing   -> ioError (userError "No valid homedir?!")
    Just home -> do
      phome  <- P.parseAbs home
      reldir <- P.parseRel ".config/hsfm"
      return $ phome P.</> reldir


bookmarksFile :: IO (Path Abs)
bookmarksFile = do
  path <- bookmarksDir
  return $ path P.</> bookmarksFileName


bookmarksFileName :: Path Fn
bookmarksFileName = fromJust $ P.parseFn "bookmarks"
