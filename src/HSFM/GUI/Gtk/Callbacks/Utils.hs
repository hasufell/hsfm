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
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

module HSFM.GUI.Gtk.Callbacks.Utils where



import Control.Monad
  (
    forM_
  , when
  )
import Data.Foldable
  (
    for_
  )
import Data.Maybe
  (
    fromJust
  )
import GHC.IO.Exception
  (
    IOErrorType(..)
  )
import Graphics.UI.Gtk
import qualified HPath as P
import HPath.IO
import HPath.IO.Errors
import HSFM.FileSystem.FileType
import qualified HSFM.FileSystem.UtilTypes as UT
import HSFM.GUI.Gtk.Data
import HSFM.GUI.Gtk.Dialogs
import HSFM.GUI.Gtk.MyView
import HSFM.History
import Prelude hiding(readFile)
import Control.Concurrent.MVar
  (
    putMVar
  , tryTakeMVar
  )




-- |Carries out a file operation with the appropriate error handling
-- allowing the user to react to various exceptions with further input.
doFileOperation :: UT.FileOperation -> IO ()
doFileOperation (UT.FCopy (UT.Copy (f':fs') to)) =
  _doFileOperation (f':fs') to (\p1 p2 cm -> easyCopy p1 p2 cm FailEarly)
    $ doFileOperation (UT.FCopy $ UT.Copy fs' to)
doFileOperation (UT.FMove (UT.Move (f':fs') to)) =
  _doFileOperation (f':fs') to moveFile
    $ doFileOperation (UT.FMove $ UT.Move fs' to)
doFileOperation _ = return ()


_doFileOperation :: [P.Path b1]
                 -> P.Path P.Abs
                 -> (P.Path b1 -> P.Path P.Abs -> CopyMode -> IO b)
                 -> IO ()
                 -> IO ()
_doFileOperation [] _ _ _ = return ()
_doFileOperation (f:fs) to mc rest = do
  toname <- P.basename f
  let topath = to P.</> toname
  reactOnError (mc f topath Strict >> rest)
    -- TODO: how safe is 'AlreadyExists' here?
    [(AlreadyExists  , collisionAction fileCollisionDialog topath)]
    [(SameFile{}     , collisionAction renameDialog topath)]
  where
    collisionAction diag topath = do
      mcm <- diag . P.fromAbs $ topath
      forM_ mcm $ \cm -> case cm of
        UT.Overwrite -> mc f topath Overwrite >> rest
        UT.OverwriteAll -> forM_ (f:fs) $ \x -> do
                          toname' <- P.basename x
                          mc x (to P.</> toname') Overwrite
        UT.Skip         -> rest
        UT.Rename newn  -> mc f (to P.</> newn) Strict >> rest
        _            -> return ()


-- |Helper that is invoked for any directory change operations.
goDir :: Bool    -- ^ whether to update the history
      -> MyGUI
      -> MyView
      -> Item
      -> IO ()
goDir bhis mygui myview item = do
  when bhis $ do
    mhs <- tryTakeMVar (history myview)
    for_ mhs $ \hs -> do
      let nhs = historyNewPath (path item) hs
      putMVar (history myview) nhs
  refreshView mygui myview item

  -- set notebook tab label
  page <- notebookGetCurrentPage (notebook myview)
  child <- fromJust <$> notebookGetNthPage (notebook myview) page

  -- get the label
  ebox <- (castToEventBox . fromJust)
    <$> notebookGetTabLabel (notebook myview) child
  label <- (castToLabel . head) <$> containerGetChildren ebox

  -- set the label
  labelSetText label
    (maybe (P.fromAbs $ path item)
           P.fromRel $ P.basename . path $ item)

