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

module GUI.Gtk.Dialogs where


import Control.Applicative
  (
    (<$>)
  )
import Control.Exception
  (
    try
  , SomeException
  )
import Control.Monad
  (
    when
  , void
  )
import Data.Version
  (
    showVersion
  )
import Distribution.Package
  (
    PackageIdentifier(..)
  , PackageName(..)
  )
import Distribution.PackageDescription
  (
    GenericPackageDescription(..)
  , PackageDescription(..)
  )
import Distribution.PackageDescription.Parse
  (
    readPackageDescription
  )
import Distribution.Verbosity
  (
    silent
  )
import Graphics.UI.Gtk
import GUI.Gtk.Data
import IO.File





    ---------------------
    --[ Dialog popups ]--
    ---------------------


-- |Pops up an error Dialog with the given String.
showErrorDialog :: String -> IO ()
showErrorDialog str = do
  errorDialog <- messageDialogNew Nothing
                                  [DialogDestroyWithParent]
                                  MessageError
                                  ButtonsClose
                                  str
  _ <- dialogRun errorDialog
  widgetDestroy errorDialog


-- |Asks the user for confirmation and returns True/False.
showConfirmationDialog :: String -> IO Bool
showConfirmationDialog str = do
  confirmDialog <- messageDialogNew Nothing
                                    [DialogDestroyWithParent]
                                    MessageQuestion
                                    ButtonsYesNo
                                    str
  rID <- dialogRun confirmDialog
  widgetDestroy confirmDialog
  case rID of
    ResponseYes -> return True
    ResponseNo  -> return False
    _           -> return False


-- |Asks the user which directory copy mode he wants via dialog popup
-- and returns 'DirCopyMode'.
showCopyModeChooserDialog :: IO DirCopyMode
showCopyModeChooserDialog = do
  chooserDialog <- messageDialogNew Nothing
                                    [DialogDestroyWithParent]
                                    MessageQuestion
                                    ButtonsNone
                                    "Choose the copy mode"
  dialogAddButton chooserDialog "Strict"  (ResponseUser 0)
  dialogAddButton chooserDialog "Merge"   (ResponseUser 1)
  dialogAddButton chooserDialog "Replace" (ResponseUser 2)
  rID <- dialogRun chooserDialog
  widgetDestroy chooserDialog
  case rID of
    ResponseUser 0 -> return Strict
    ResponseUser 1 -> return Merge
    ResponseUser 2 -> return Replace


-- |Shows the about dialog from the help menu.
showAboutDialog :: IO ()
showAboutDialog = do
  ad       <- aboutDialogNew
  lstr     <- readFile "LICENSE"
  hsfmicon <- pixbufNewFromFile "data/Gtk/icons/hsfm.png"
  pdesc    <- packageDescription <$> readPackageDescription silent "hsfm.cabal"
  set ad
    [ aboutDialogProgramName  := (unPackageName . pkgName . package) pdesc
    , aboutDialogName         := (unPackageName . pkgName . package) pdesc
    , aboutDialogVersion      := (showVersion . pkgVersion . package) pdesc
    , aboutDialogCopyright    := copyright pdesc
    , aboutDialogComments     := description pdesc
    , aboutDialogLicense      := Just lstr
    , aboutDialogWebsite      := homepage pdesc
    , aboutDialogAuthors      := [author pdesc]
    , aboutDialogLogo         := Just hsfmicon
    , aboutDialogWrapLicense  := True
    ]
  _ <- dialogRun ad
  widgetDestroy ad


-- |Carry out an IO action with a confirmation dialog.
-- If the user presses "No", then do nothing.
withConfirmationDialog :: String -> IO () -> IO ()
withConfirmationDialog str io = do
  run <- showConfirmationDialog str
  when run io


-- |Execute the given IO action. If the action throws exceptions,
-- visualize them via 'showErrorDialog'.
withErrorDialog :: IO a -> IO ()
withErrorDialog io = do
  r <- try io
  either (\e -> showErrorDialog $ show (e :: SomeException))
         (\_ -> return ())
         r


-- |Asks the user which directory copy mode he wants via dialog popup
-- and returns 'DirCopyMode'.
textInputDialog :: String -> IO (Maybe String)
textInputDialog title = do
  chooserDialog <- messageDialogNew Nothing
                                    [DialogDestroyWithParent]
                                    MessageQuestion
                                    ButtonsNone
                                    title
  entry <- entryNew
  cbox <- dialogGetActionArea chooserDialog
  dialogAddButton chooserDialog "Ok"     (ResponseUser 0)
  dialogAddButton chooserDialog "Cancel" (ResponseUser 1)
  boxPackStart (castToBox cbox) entry PackNatural 5
  widgetShowAll chooserDialog
  rID <- dialogRun chooserDialog
  ret <- case rID of
           -- TODO: make this more safe
           ResponseUser 0 -> Just <$> entryGetText entry
           ResponseUser 1 -> return Nothing
  widgetDestroy chooserDialog
  return ret
