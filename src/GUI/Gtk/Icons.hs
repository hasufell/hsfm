{-# OPTIONS_HADDOCK ignore-exports #-}

module GUI.Gtk.Icons where


import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Pixbuf


-- |Icon type we use in our GUI.
data GtkIcon = IFolder
             | IFile
             | IError


-- |Gets an icon from the default icon theme and falls back to project-icons
-- if not found. The requested icon size is not guaranteed.
getIcon :: GtkIcon    -- ^ icon we want
        -> Int        -- ^ requested icon size
        -> IO Pixbuf
getIcon icon isize = do
  let iname = iconToStr icon
  iT <- iconThemeGetDefault
  mpix <- iconThemeLoadIcon iT iname isize IconLookupUseBuiltin
  case mpix of
    Just pix -> return pix
    Nothing  -> pixbufNewFromFile ("data/Gtk/icons/" ++ iname)
  where
    iconToStr IFolder = "gtk-directory"
    iconToStr IFile   = "gtk-file"
    iconToStr IError  = "error"
