{--
HSFM, a filemanager written in Haskell.
Copyright (C) 2015 Julian Ospald

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

-- |Module for Gtk icon handling.
module GUI.Gtk.Icons where


import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Pixbuf


-- |Icon type we use in our GUI.
data GtkIcon = IFolder
             | SymL
             | IFile
             | IError


-- |Gets an icon from the default icon theme and falls back to project-icons
-- if not found. The requested icon size is not guaranteed.
getIcon :: GtkIcon    -- ^ icon we want
        -> IconTheme  -- ^ which icon theme to get the icon from
        -> Int        -- ^ requested icon size
        -> IO Pixbuf
getIcon icon itheme isize = do
  let iname = iconToStr icon
  mpix <- iconThemeLoadIcon itheme iname isize IconLookupUseBuiltin
  case mpix of
    Just pix -> return pix
    Nothing  -> pixbufNewFromFile ("data/Gtk/icons/" ++ iname)
  where
    iconToStr IFolder = "gtk-directory"
    iconToStr IFile   = "gtk-file"
    iconToStr IError  = "error"
    iconToStr SymL    = "emblem-symbolic-link"


getSymlinkIcon :: GtkIcon -> IconTheme -> Int -> IO Pixbuf
getSymlinkIcon icon itheme isize = do
  pix    <- pixbufCopy =<< getIcon icon itheme isize
  sympix <- getIcon SymL itheme isize

  pixbufScale sympix pix 0 0 12 12 0 0 0.5 0.5 InterpNearest

  return pix
