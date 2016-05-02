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


import Control.Exception
  (
    catch
  , displayException
  , throw
  , IOException
  , catches
  , Handler(..)
  )
import Control.Monad
  (
    forM
  , when
  , void
  )
import qualified Data.ByteString as BS
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
import HSFM.FileSystem.Errors
import HSFM.FileSystem.FileType
import HSFM.GUI.Glib.GlibString()
import HSFM.GUI.Gtk.Data
import HSFM.GUI.Gtk.Errors
import Paths_hsfm
  (
    getDataFileName
  )



-- |Copy modes.
data CopyMode = Strict  -- ^ fail if the target already exists
              | Merge   -- ^ overwrite files if necessary, for files, this
                        --   is the same as Replace
              | Replace -- ^ remove targets before copying, this is
                        --   only useful if the target is a directorty
              | Rename (P.Path P.Fn)




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
-- and returns 'DirCopyMode'. Default is always Strict, so this allows
-- switching to Merge/Replace/Rename.
showCopyModeDialog :: IO (Maybe CopyMode)
showCopyModeDialog = do
  chooserDialog <- messageDialogNew Nothing
                                    [DialogDestroyWithParent]
                                    MessageQuestion
                                    ButtonsNone
                                    "Target exists, how to proceed?"
  _ <- dialogAddButton chooserDialog "Cancel"  (ResponseUser 0)
  _ <- dialogAddButton chooserDialog "Merge"   (ResponseUser 1)
  _ <- dialogAddButton chooserDialog "Replace" (ResponseUser 2)
  _ <- dialogAddButton chooserDialog "Rename"  (ResponseUser 3)
  rID <- dialogRun chooserDialog
  widgetDestroy chooserDialog
  case rID of
    ResponseUser 0 -> return Nothing
    ResponseUser 1 -> return (Just Merge)
    ResponseUser 2 -> return (Just Replace)
    ResponseUser 3 -> do
      mfn   <- textInputDialog "Enter new name"
      forM mfn $ \fn -> do
        pfn <- P.parseFn (P.userStringToFP fn)
        return $ Rename pfn
    _              -> throw  UnknownDialogButton


-- |Stipped version of `showCopyModeDialog` that only allows cancelling
-- or Renaming.
showRenameDialog :: IO (Maybe CopyMode)
showRenameDialog = do
  chooserDialog <- messageDialogNew Nothing
                                    [DialogDestroyWithParent]
                                    MessageQuestion
                                    ButtonsNone
                                    "Target exists, how to proceed?"
  _ <- dialogAddButton chooserDialog "Cancel"  (ResponseUser 0)
  _ <- dialogAddButton chooserDialog "Rename"  (ResponseUser 1)
  rID <- dialogRun chooserDialog
  widgetDestroy chooserDialog
  case rID of
    ResponseUser 0 -> return Nothing
    ResponseUser 1 -> do
      mfn   <- textInputDialog "Enter new name"
      forM mfn $ \fn -> do
        pfn <- P.parseFn (P.userStringToFP fn)
        return $ Rename pfn
    _              -> throw  UnknownDialogButton


-- |Attempts to run the given function with the `Strict` copy mode.
-- If that raises a `FileDoesExist` or `DirDoesExist`, then it prompts
-- the user for action via `showCopyModeDialog` and then carries out
-- the given function again.
withCopyModeDialog :: (CopyMode -> IO ()) -> IO ()
withCopyModeDialog fa =
  catch (fa Strict) $ \e ->
    case e of
      FileDoesExist _ -> doIt showCopyModeDialog
      DirDoesExist  _ -> doIt showCopyModeDialog
      SameFile _ _    -> doIt showRenameDialog
      e'              -> throw e'
  where
    doIt getCm = do
      mcm <- getCm
      case mcm of
        (Just Strict) -> return () -- don't try again
        (Just cm)     -> fa cm
        Nothing       -> return ()


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
                       $ displayException (e :: IOException))
    , Handler (\e -> showErrorDialog
                       $ displayException (e :: FmIOException))
    ]


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
  _ <- dialogAddButton chooserDialog "Ok"     (ResponseUser 0)
  _ <- dialogAddButton chooserDialog "Cancel" (ResponseUser 1)
  boxPackStart (castToBox cbox) entry PackNatural 5
  widgetShowAll chooserDialog
  rID <- dialogRun chooserDialog
  ret <- case rID of
           -- TODO: make this more safe
           ResponseUser 0 -> Just <$> entryGetText entry
           ResponseUser 1 -> return Nothing
           _              -> throw  UnknownDialogButton
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

