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

module GUI.Gtk.Callbacks where


import Control.Applicative
  (
    (<$>)
  , (<*>)
  )
import Control.Concurrent.STM
  (
    TVar
  , newTVarIO
  , readTVarIO
  )
import Control.Exception
  (
    throw
  )
import Control.Monad
  (
    void
  , forM_
  )
import Control.Monad.IO.Class
  (
    liftIO
  )
import Data.DirTree
import Data.Foldable
  (
    for_
  )
import Graphics.UI.Gtk
import GUI.Gtk.Data
import GUI.Gtk.Dialogs
import GUI.Gtk.Utils
import IO.Error
import IO.File
import IO.Utils
import System.FilePath
  (
    isAbsolute
  , (</>)
  )
import System.Glib.UTFString
  (
    glibToString
  )





    -----------------
    --[ Callbacks ]--
    -----------------


-- |Set callbacks, on hotkeys, events and stuff.
--
-- Interaction with mutable references:
--
-- * 'settings mygui' modifies
setCallbacks :: MyGUI -> MyView -> IO ()
setCallbacks mygui myview = do
  -- GUI events
  _ <- urlBar mygui `on` entryActivated $ urlGoTo mygui myview
  _ <- treeView mygui `on` rowActivated $ (\_ _ -> withRows mygui myview open)
  _ <- refreshView mygui `on` buttonActivated $ do
       cdir <- liftIO $ getCurrentDir myview
       refreshTreeView' mygui myview cdir
  _ <- clearStatusBar mygui `on` buttonActivated $ do
       popStatusbar mygui
       writeTVarIO (operationBuffer myview) None

  -- key events
  _ <- rootWin mygui `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "q"       <- fmap glibToString eventKeyName
    liftIO mainQuit
  _ <- treeView mygui `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "h"       <- fmap glibToString eventKeyName
    cdir <- liftIO $ getCurrentDir myview
    liftIO $ modifyTVarIO (settings mygui)
                          (\x -> x { showHidden = not . showHidden $ x})
             >> refreshTreeView' mygui myview cdir
  _ <- treeView mygui `on` keyPressEvent $ tryEvent $ do
    [Alt] <- eventModifier
    "Up"  <- fmap glibToString eventKeyName
    liftIO $ upDir mygui myview
  _ <- treeView mygui `on` keyPressEvent $ tryEvent $ do
    "Delete"  <- fmap glibToString eventKeyName
    liftIO $ withRows mygui myview del
  _ <- treeView mygui `on` keyPressEvent $ tryEvent $ do
    []            <- eventModifier
    "Return"      <- fmap glibToString eventKeyName
    liftIO $ withRows mygui myview open
  _ <- treeView mygui `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "c"       <- fmap glibToString eventKeyName
    liftIO $ withRows mygui myview copyInit
  _ <- treeView mygui `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "x"       <- fmap glibToString eventKeyName
    liftIO $ withRows mygui myview moveInit
  _ <- treeView mygui `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "v"       <- fmap glibToString eventKeyName
    liftIO $ operationFinal mygui myview

  -- menubar-file
  _ <- menubarFileQuit mygui `on` menuItemActivated $ mainQuit
  _ <- menubarFileOpen mygui `on` menuItemActivated $
    liftIO $ withRows mygui myview open
  _ <- menubarFileExecute mygui `on` menuItemActivated $
    liftIO $ withRows mygui myview execute
  _ <- menubarFileNew mygui `on` menuItemActivated $
    liftIO $ newFile mygui myview

  -- menubar-edit
  _ <- menubarEditCut mygui `on` menuItemActivated $
    liftIO $ withRows mygui myview moveInit
  _ <- menubarEditCopy mygui `on` menuItemActivated $
    liftIO $ withRows mygui myview copyInit
  _ <- menubarEditRename mygui `on` menuItemActivated $
    liftIO $ withRows mygui myview renameF
  _ <- menubarEditPaste mygui `on` menuItemActivated $
    liftIO $ operationFinal mygui myview
  _ <- menubarEditDelete mygui `on` menuItemActivated $
    liftIO $ withRows mygui myview del

  -- menubar-help
  _ <- menubarHelpAbout mygui `on` menuItemActivated $
    liftIO showAboutDialog

  -- righ-click
  _ <- treeView mygui `on` buttonPressEvent $ do
    eb <- eventButton
    t  <- eventTime
    case eb of
      RightButton -> liftIO $ menuPopup (rcMenu mygui) $ Just (RightButton, t)
      _           -> return ()
    return False
  _ <- rcFileOpen mygui `on` menuItemActivated $
    liftIO $ withRows mygui myview open
  _ <- rcFileExecute mygui `on` menuItemActivated $
    liftIO $ withRows mygui myview execute
  _ <- rcFileNew mygui `on` menuItemActivated $
    liftIO $ newFile mygui myview
  _ <- rcFileCopy mygui `on` menuItemActivated $
    liftIO $ withRows mygui myview copyInit
  _ <- rcFileRename mygui `on` menuItemActivated $
    liftIO $ withRows mygui myview renameF
  _ <- rcFilePaste mygui `on` menuItemActivated $
    liftIO $ operationFinal mygui myview
  _ <- rcFileDelete mygui `on` menuItemActivated $
    liftIO $ withRows mygui myview del
  _ <- rcFileCut mygui `on` menuItemActivated $
    liftIO $ withRows mygui myview moveInit

  return ()


-- |Go to the url given at the 'urlBar' and visualize it in the given
-- treeView.
urlGoTo :: MyGUI -> MyView -> IO ()
urlGoTo mygui myview = withErrorDialog $ do
  fp <- entryGetText (urlBar mygui)
  let abs = isAbsolute fp
  exists <- (||) <$> doesDirectoryExist fp <*> doesFileExist fp
  -- TODO: more explicit error handling?
  refreshTreeView mygui myview (Just fp)


-- |Supposed to be used with 'withRows'. Opens a file or directory.
open :: [Row] -> MyGUI -> MyView -> IO ()
open [row] mygui myview = withErrorDialog $
  case row of
    ADirOrSym r -> do
      nv <- Data.DirTree.readFile $ fullPath r
      refreshTreeView' mygui myview nv
    r ->
      void $ openFile r
-- this throws on the first error that occurs
open (FileLikeList fs) mygui myview = withErrorDialog $
  forM_ fs $ \f -> void $ openFile f
open _ _ _ = withErrorDialog
               . throw $ InvalidOperation
                         "Operation not supported on multiple files"


-- |Execute a given file.
execute :: [Row] -> MyGUI -> MyView -> IO ()
execute [row] mygui myview = withErrorDialog $
  void $ executeFile row []
execute _ _ _ = withErrorDialog
                  . throw $ InvalidOperation
                            "Operation not supported on multiple files"


-- |Supposed to be used with 'withRows'. Deletes a file or directory.
del :: [Row] -> MyGUI -> MyView -> IO ()
del [row] mygui myview = withErrorDialog $ do
  let cmsg  = "Really delete \"" ++ fullPath row ++ "\"?"
  withConfirmationDialog cmsg
    $ easyDelete row
-- this throws on the first error that occurs
del rows@(_:_) mygui myview = withErrorDialog $ do
  let cmsg  = "Really delete " ++ show (length rows) ++ " files?"
  withConfirmationDialog cmsg
    $ forM_ rows $ \row -> easyDelete row
del _ _ _ = withErrorDialog
              . throw $ InvalidOperation
                        "Operation not supported on multiple files"


-- |Initializes a file move operation.
--
-- Interaction with mutable references:
--
-- * 'operationBuffer' writes
moveInit :: [Row] -> MyGUI -> MyView -> IO ()
moveInit [row] mygui myview = do
  writeTVarIO (operationBuffer myview) (FMove . MP1 $ row)
  let sbmsg = "Move buffer: " ++ fullPath row
  void $ pushStatusBar mygui sbmsg
moveInit _ _ _ = withErrorDialog
                   . throw $ InvalidOperation
                             "Operation not supported on multiple files"

-- |Supposed to be used with 'withRows'. Initializes a file copy operation.
--
-- Interaction with mutable references:
--
-- * 'operationBuffer' writes
copyInit :: [Row] -> MyGUI -> MyView -> IO ()
copyInit [row] mygui myview = do
  writeTVarIO (operationBuffer myview) (FCopy . CP1 $ row)
  let sbmsg = "Copy buffer: " ++ fullPath row
  void $ pushStatusBar mygui sbmsg
copyInit _ _ _ = withErrorDialog
                   . throw $ InvalidOperation
                             "Operation not supported on multiple files"


-- |Finalizes a file operation, such as copy or move.
--
-- Interaction with mutable references:
--
-- * 'operationBuffer' reads
operationFinal :: MyGUI -> MyView -> IO ()
operationFinal mygui myview = withErrorDialog $ do
  op <- readTVarIO (operationBuffer myview)
  cdir <- getCurrentDir myview
  case op of
    FMove (MP1 s) -> do
      let cmsg = "Really move \"" ++ fullPath s
                  ++ "\"" ++ " to \"" ++ fullPath cdir ++ "\"?"
      withConfirmationDialog cmsg . withCopyModeDialog
        $ \cm -> void $ runFileOp (FMove . MC s cdir $ cm)
      return ()
    FCopy (CP1 s) -> do
      let cmsg = "Really copy \"" ++ fullPath s
                 ++ "\"" ++ " to \"" ++ fullPath cdir ++ "\"?"
      withConfirmationDialog cmsg . withCopyModeDialog
        $ \cm -> void $ runFileOp (FCopy . CC s cdir $ cm)
      return ()
    _ -> return ()


-- |Go up one directory and visualize it in the treeView.
--
-- Interaction with mutable references:
--
-- * 'rawModel' reads
-- * 'sortedModel' reads
upDir :: MyGUI -> MyView -> IO ()
upDir mygui myview = withErrorDialog $ do
  cdir <- getCurrentDir myview
  rawModel'    <- readTVarIO $ rawModel myview
  sortedModel' <- readTVarIO $ sortedModel myview
  nv <- goUp cdir
  refreshTreeView' mygui myview nv


-- |Go up one directory and visualize it in the treeView.
newFile :: MyGUI -> MyView -> IO ()
newFile mygui myview = withErrorDialog $ do
  mfn   <- textInputDialog "Enter file name"
  for_ mfn $ \fn -> do
    cdir  <- getCurrentDir myview
    createFile cdir fn


renameF :: [Row] -> MyGUI -> MyView -> IO ()
renameF [row] mygui myview = withErrorDialog $ do
  mfn   <- textInputDialog "Enter new file name"
  for_ mfn $ \fn -> do
    let cmsg = "Really rename \"" ++ fullPath row
               ++ "\"" ++ " to \"" ++ anchor row </> fn ++ "\"?"
    withConfirmationDialog cmsg $ IO.File.renameFile row fn
renameF _ _ _ = withErrorDialog
                  . throw $ InvalidOperation
                            "Operation not supported on multiple files"
