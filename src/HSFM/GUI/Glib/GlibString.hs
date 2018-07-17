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


module HSFM.GUI.Glib.GlibString where


import qualified Data.ByteString as BS
import Data.ByteString.UTF8
  (
    toString
  )
import Data.Word8
  (
    _percent
  )
import Foreign.C.String
  (
    CStringLen
  , CString
  )
import Foreign.C.Types
  (
    CSize(..)
  )
import Foreign.Marshal.Utils
  (
    maybePeek
  )
import Foreign.Ptr
  (
    nullPtr
  , plusPtr
  )
import System.Glib.UTFString



-- TODO: move this to its own module
instance GlibString BS.ByteString where
    withUTFString = BS.useAsCString
    withUTFStringLen s f = BS.useAsCStringLen  s (f . noNullPtrs)
    peekUTFString s = do
        len <- c_strlen s
        BS.packCStringLen (s, fromIntegral len)
    maybePeekUTFString = maybePeek peekUTFString
    peekUTFStringLen = BS.packCStringLen
    newUTFString = newUTFString . toString
    newUTFStringLen = newUTFStringLen . toString
    genUTFOfs = genUTFOfs . toString
    stringLength = BS.length
    unPrintf s = BS.intercalate (BS.pack [_percent, _percent]) (BS.split _percent s)


foreign import ccall unsafe "string.h strlen" c_strlen
    :: CString -> IO CSize


noNullPtrs :: CStringLen -> CStringLen
noNullPtrs (p, 0) | p == nullPtr = (plusPtr p 1, 0)
noNullPtrs s = s

