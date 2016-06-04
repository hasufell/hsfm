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
import HSFM.FileSystem.UtilTypes
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
doFileOperation :: FileOperation -> IO ()
doFileOperation (FCopy (Copy (f':fs') to)) =
  _doFileOperation (f':fs') to easyCopyOverwrite easyCopy
    $ doFileOperation (FCopy $ Copy fs' to)
doFileOperation (FMove (Move (f':fs') to)) =
  _doFileOperation (f':fs') to moveFileOverwrite moveFile
    $ doFileOperation (FMove $ Move fs' to)
doFileOperation _ = return ()


_doFileOperation :: [P.Path b1]
                 -> P.Path P.Abs
                 -> (P.Path b1 -> P.Path P.Abs -> IO b)
                 -> (P.Path b1 -> P.Path P.Abs -> IO a)
                 -> IO ()
                 -> IO ()
_doFileOperation [] _ _ _ _ = return ()
_doFileOperation (f:fs) to mcOverwrite mc rest = do
  toname <- P.basename f
  let topath = to P.</> toname
  reactOnError (mc f topath >> rest)
    [(AlreadyExists  , collisionAction fileCollisionDialog topath)]
    [(FileDoesExist{}, collisionAction fileCollisionDialog topath)
    ,(DirDoesExist{} , collisionAction fileCollisionDialog topath)
    ,(SameFile{}     , collisionAction renameDialog topath)]
  where
    collisionAction diag topath = do
      mcm <- diag . P.fromAbs $ topath
      forM_ mcm $ \cm -> case cm of
        Overwrite    -> mcOverwrite f topath >> rest
        OverwriteAll -> forM_ (f:fs) $ \x -> do
                          toname' <- P.basename x
                          mcOverwrite x (to P.</> toname')
        Skip         -> rest
        Rename newn  -> mc f (to P.</> newn) >> rest
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
      let nhs = goNewPath (path item) hs
      putMVar (history myview) nhs
  refreshView mygui myview item

  -- set notebook tab label
  page <- notebookGetCurrentPage (notebook mygui)
  child <- fromJust <$> notebookGetNthPage (notebook mygui) page

  -- get the label
  ebox <- (castToEventBox . fromJust)
    <$> notebookGetTabLabel (notebook mygui) child
  label <- (castToLabel . head) <$> containerGetChildren ebox

  -- set the label
  labelSetText label
    (maybe (P.fromAbs $ path item)
           P.fromRel $ P.basename . path $ item)

