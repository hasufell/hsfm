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


{-# LANGUAGE TupleSections #-}
{-# OPTIONS_HADDOCK ignore-exports #-}


module HSFM.GUI.Gtk.Plugins where


import Graphics.UI.Gtk
import HPath
import HSFM.FileSystem.FileType
import HSFM.GUI.Gtk.Data
import HSFM.GUI.Gtk.Settings
import HSFM.GUI.Gtk.Utils
import HSFM.Settings
import Control.Monad
  (
    forM
  , forM_
  , void
  )
import System.Posix.Process.ByteString
  (
    executeFile
  , forkProcess
  )
import Data.ByteString.UTF8
  (
    fromString
  )
import qualified Data.ByteString as BS




    ---------------
    --[ Plugins ]--
    ---------------



---- Init functions ----


-- |Usually, you don't want to overwrite this method. It
-- adds plugins from `myplugins` and initializes the callbacks
-- automatically.
addPlugins :: MyGUI -> MyView -> IO ()
addPlugins mygui myview
  | null myplugins = return ()
  | otherwise = do
    -- add another plugin separator after the existing one
    -- where we want to place our plugins
    sep2 <- separatorMenuItemNew
    widgetShow sep2

    menuShellInsert (rcMenu . rcmenu $ myview) sep2 insertPos

    plugins <- forM myplugins $ \(ma,mb) -> fmap (,mb) ma
    -- need to reverse plugins list so the order is right
    forM_ (reverse plugins) $ \(plugin, cb) -> do
      menuShellInsert (rcMenu . rcmenu $ myview) plugin insertPos
      widgetShow plugin
      -- init callback
      plugin `on` menuItemActivated $ withItems mygui myview cb

    return ()





---- Global settings ----



-- |Where to start inserting plugins.
insertPos :: Int
insertPos = 4


-- |A list of plugins to add to the right-click menu at position
-- `insertPos`. The left part of the tuple is the menuitem, the right
-- part the callback.
-- Plugins are added in order of this list.
myplugins :: [(IO MenuItem, [Item] -> MyGUI -> MyView -> IO ())]
myplugins = [(diffPlugin, diffCallback)
            ]





---- The plugins ----



diffPlugin :: IO MenuItem
diffPlugin = menuItemNewWithLabel "diff"

diffCallback :: [Item] -> MyGUI -> MyView -> IO ()
diffCallback items _ _ = void $
  forkProcess $
    executeFile
      (fromString "meld")
      True
      ([fromString "--diff"] ++ fmap (fromAbs . path) items)
      Nothing

