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

module HSFM.GUI.Gtk.Dialogs where


import Codec.Binary.UTF8.String
  (
    decodeString
  )
import Control.Exception
  (
    catches
  , displayException
  , throwIO
  , IOException
  , Handler(..)
  )
import Control.Monad
  (
    forM
  , when
  , void
  )
import Data.ByteString
  (
    ByteString
  )
import qualified Data.ByteString as BS
import Data.ByteString.UTF8
  (
    fromString
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
import qualified HPath as P
import HPath.IO.Errors
import HSFM.FileSystem.FileType
import HSFM.FileSystem.UtilTypes
import HSFM.GUI.Glib.GlibString()
import HSFM.GUI.Gtk.Data
import HSFM.GUI.Gtk.Errors
import Paths_hsfm
  (
    getDataFileName
  )
import System.Glib.UTFString
  (
    GlibString
  )
import System.Posix.FilePath
  (
    takeFileName
  )








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


fileCollisionDialog :: ByteString -> IO (Maybe FCollisonMode)
fileCollisionDialog t = do
  chooserDialog <- messageDialogNew Nothing
                                    [DialogDestroyWithParent]
                                    MessageQuestion
                                    ButtonsNone
                                    (fromString "Target \"" `BS.append`
                                     t `BS.append`
                                     fromString "\" exists, how to proceed?")
  _ <- dialogAddButton chooserDialog "Cancel"        (ResponseUser 0)
  _ <- dialogAddButton chooserDialog "Overwrite"     (ResponseUser 1)
  _ <- dialogAddButton chooserDialog "Overwrite all" (ResponseUser 2)
  _ <- dialogAddButton chooserDialog "Skip"          (ResponseUser 3)
  _ <- dialogAddButton chooserDialog "Rename"        (ResponseUser 4)
  rID <- dialogRun chooserDialog
  widgetDestroy chooserDialog
  case rID of
    ResponseUser 0 -> return Nothing
    ResponseUser 1 -> return (Just Overwrite)
    ResponseUser 2 -> return (Just OverwriteAll)
    ResponseUser 3 -> return (Just Skip)
    ResponseUser 4 -> do
      mfn   <- textInputDialog (fromString "Enter new name") (takeFileName t)
      forM mfn $ \fn -> do
        pfn <- P.parseFn (fromString fn)
        return $ Rename pfn
    _              -> throwIO UnknownDialogButton


renameDialog :: ByteString -> IO (Maybe FCollisonMode)
renameDialog t = do
  chooserDialog <- messageDialogNew Nothing
                                    [DialogDestroyWithParent]
                                    MessageQuestion
                                    ButtonsNone
                                    (fromString "Target \"" `BS.append`
                                     t `BS.append`
                                     fromString "\" exists, how to proceed?")
  _ <- dialogAddButton chooserDialog "Cancel"        (ResponseUser 0)
  _ <- dialogAddButton chooserDialog "Skip"          (ResponseUser 1)
  _ <- dialogAddButton chooserDialog "Rename"        (ResponseUser 2)
  rID <- dialogRun chooserDialog
  widgetDestroy chooserDialog
  case rID of
    ResponseUser 0 -> return Nothing
    ResponseUser 1 -> return (Just Skip)
    ResponseUser 2 -> do
      mfn   <- textInputDialog (fromString "Enter new name") (takeFileName t)
      forM mfn $ \fn -> do
        pfn <- P.parseFn (fromString fn)
        return $ Rename pfn
    _              -> throwIO UnknownDialogButton


-- |Shows the about dialog from the help menu.
showAboutDialog :: IO ()
showAboutDialog = do
  ad       <- aboutDialogNew
  lstr     <- Prelude.readFile =<< getDataFileName "LICENSE"
  hsfmicon <- pixbufNewFromFile =<< getDataFileName "data/Gtk/icons/hsfm.png"
  pdesc    <- fmap packageDescription
                   (readPackageDescription silent
                     =<< getDataFileName "hsfm.cabal")
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
withErrorDialog io =
  catches (void io)
    [ Handler (\e -> showErrorDialog
                       . decodeString
                       . displayException
                       $ (e :: IOException))
    , Handler (\e -> showErrorDialog
                       $ displayException (e :: HPathIOException))
    ]


-- |Asks the user which directory copy mode he wants via dialog popup
-- and returns 'DirCopyMode'.
textInputDialog :: GlibString string
                => string   -- ^ window title
                -> string   -- ^ initial text in input widget
                -> IO (Maybe String)
textInputDialog title inittext = do
  chooserDialog <- messageDialogNew Nothing
                                    [DialogDestroyWithParent]
                                    MessageQuestion
                                    ButtonsNone
                                    title
  entry <- entryNew
  entrySetText entry inittext
  cbox <- dialogGetActionArea chooserDialog
  _ <- dialogAddButton chooserDialog "Ok"     (ResponseUser 0)
  _ <- dialogAddButton chooserDialog "Cancel" (ResponseUser 1)
  boxPackStart (castToBox cbox) entry PackNatural 5
  widgetShowAll chooserDialog
  rID <- dialogRun chooserDialog
  ret <- case rID of
           -- TODO: make this more safe
           ResponseUser 0 -> Just <$> entryGetText entry
           ResponseUser 1 -> return Nothing
           _              -> throwIO UnknownDialogButton
  widgetDestroy chooserDialog
  return ret


showFilePropertyDialog :: [Item] -> MyGUI -> MyView -> IO ()
showFilePropertyDialog [item] mygui _ = do
  dialog <- messageDialogNew Nothing
                             [DialogDestroyWithParent]
                             MessageInfo
                             ButtonsNone
                             "File Properties"

  let fprop' = fprop mygui
      grid   = fpropGrid fprop'

  entrySetText (fpropFnEntry fprop')  (maybe BS.empty P.fromRel
                                        $ P.basename . path $ item)
  entrySetText (fpropLocEntry fprop') (P.fromAbs . P.dirname . path $ item)
  entrySetText (fpropTsEntry fprop')  (fromFreeVar (show . fileSize) item)
  entrySetText (fpropModEntry fprop') (packModTime item)
  entrySetText (fpropAcEntry fprop')  (packAccessTime item)
  entrySetText (fpropFTEntry fprop')  (packFileType item)
  entrySetText (fpropPermEntry fprop')
               (tail $ packPermissions item) -- throw away the filetype part
  case packLinkDestination item of
    (Just dest) -> do
      widgetSetSensitive (fpropLDEntry fprop') True
      entrySetText (fpropLDEntry fprop') dest
    Nothing     -> do
      widgetSetSensitive (fpropLDEntry fprop') False
      entrySetText (fpropLDEntry fprop') "( Not a symlink )"


  cbox <- dialogGetActionArea dialog
  _ <- dialogAddButton dialog "Ok"     (ResponseUser 0)
  _ <- dialogAddButton dialog "Cancel" (ResponseUser 1)
  boxPackStart (castToBox cbox) grid PackNatural 5

  widgetShowAll dialog
  _ <- dialogRun dialog

  -- make sure our grid does not get destroyed
  containerRemove (castToBox cbox) grid

  widgetDestroy dialog

  return ()
showFilePropertyDialog _ _ _ = return ()

