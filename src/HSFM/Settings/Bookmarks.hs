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
  )
import Data.Word8
  (
    _lf
  , _nul
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



-- |A bookmark. `bkName` is principally a description of the bookmark
-- but must satisfy the constraints of a filename.
data Bookmark = MkBookmark {
    bkName :: Path Fn
  , bkPath :: Path Abs
} deriving (Show)


-- |Parses bookmarks from a ByteString that has pairs of
-- name and path. Name and path are separated by null character
-- and the pairs istelf are separated by newline `_lf` from
-- each other.
bkParser :: Parser [Bookmark]
bkParser =
  fmap catMaybes $ many' (fmap toBm $ bookmark <* word8 _lf)
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
    char = satisfy (`notElem` [_nul, _lf])


-- |Writes bookmarks to a give file.
writeBookmarksToFile :: Path Abs -> [Bookmark] -> IO ()
writeBookmarksToFile _ [] = return ()
writeBookmarksToFile p bs = do
  let str = foldr1 (\x y -> x `BS.append` BS.singleton _lf
                              `BS.append`
                            y `BS.append` BS.singleton _lf)
            (fmap toByteString bs)
  file <- readFile getFileInfo p
  void $ writeFile file str
  where
    toByteString :: Bookmark -> ByteString
    toByteString b =             (P.fromRel $ bkName b)
                     `BS.append` BS.singleton _nul
                     `BS.append` (P.fromAbs $ bkPath b)


-- |Reads bookmarks from a given file.
readBookmarksFromFile :: Path Abs -> IO [Bookmark]
readBookmarksFromFile p = do
  file <- readFile getFileInfo p
  c    <- readFileContents file
  case parseOnly bkParser c of
    Left _  -> return []
    Right x -> return x

