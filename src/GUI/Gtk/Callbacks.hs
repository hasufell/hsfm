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
import Control.Monad
  (
    void
  )
import Control.Monad.IO.Class
  (
    liftIO
  )
import Data.DirTree
import Graphics.UI.Gtk
import GUI.Gtk.Data
import GUI.Gtk.Dialogs
import GUI.Gtk.Utils
import IO.File
import IO.Utils
import System.Directory
  (
    doesFileExist
  , doesDirectoryExist
  )
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
  _ <- rootWin mygui `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "q"       <- fmap glibToString eventKeyName
    liftIO mainQuit
  _ <- treeView mygui `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "h"       <- fmap glibToString eventKeyName
    mcdir <- liftIO $ getFirstRow myview
    liftIO $ modifyTVarIO (settings mygui)
                          (\x -> x { showHidden = not . showHidden $ x})
             >> refreshTreeView' mygui myview mcdir
  _ <- treeView mygui `on` keyPressEvent $ tryEvent $ do
    [Alt] <- eventModifier
    "Up"  <- fmap glibToString eventKeyName
    liftIO $ upDir mygui myview
  _ <- treeView mygui `on` keyPressEvent $ tryEvent $ do
    "Delete"  <- fmap glibToString eventKeyName
    liftIO $ withRow mygui myview del
  _ <- treeView mygui `on` rowActivated $ (\_ _ -> withRow mygui myview open)
  _ <- menubarFileQuit mygui `on` menuItemActivated $ mainQuit
  _ <- urlBar mygui `on` entryActivated $ urlGoTo mygui myview
  _ <- treeView mygui `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "c"       <- fmap glibToString eventKeyName
    liftIO $ withRow mygui myview copyInit
  _ <- treeView mygui `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "v"       <- fmap glibToString eventKeyName
    liftIO $ copyFinal mygui myview
  return ()


-- |Go to the url given at the 'urlBar' and visualize it in the given
-- treeView.
urlGoTo :: MyGUI -> MyView -> IO ()
urlGoTo mygui myview = do
  fp <- entryGetText (urlBar mygui)
  let abs = isAbsolute fp
  exists <- (||) <$> doesDirectoryExist fp <*> doesFileExist fp
  -- TODO: more explicit error handling?
  refreshTreeView mygui myview (Just fp)


-- |Supposed to be used with 'withRow'. Opens a file or directory.
open :: Row -> MyGUI -> MyView -> IO ()
open row mygui myview = withErrorDialog $
  case row of
    r@(_ :/ Dir _ _) -> do
      nv <- Data.DirTree.readFile $ fullPath r
      refreshTreeView' mygui myview nv
    r ->
      void $ openFile r


-- |Supposed to be used with 'withRow'. Deletes a file or directory.
del :: Row -> MyGUI -> MyView -> IO ()
del row mygui myview = withErrorDialog $ do
  let cmsg  = "Really delete \"" ++ fullPath row ++ "\"?"
  withConfirmationDialog cmsg
    $ easyDelete row >> refreshTreeView mygui myview Nothing


-- |Supposed to be used with 'withRow'. Initializes a file copy operation.
--
-- Interaction with mutable references:
--
-- * 'operationBuffer' writes
copyInit :: Row -> MyGUI -> MyView -> IO ()
copyInit row mygui myview =
  writeTVarIO (operationBuffer myview) (FCopy . CP1 $ row)


-- |Finalizes a file copy operation.
--
-- Interaction with mutable references:
--
-- * 'operationBuffer' reads
copyFinal :: MyGUI -> MyView -> IO ()
copyFinal mygui myview = withErrorDialog $ do
  op <- readTVarIO (operationBuffer myview)
  mcdir <- getFirstRow myview
  case op of
    FCopy (CP1 s) -> do
      dest <- goUp mcdir
      let cmsg = "Really copy \"" ++ fullPath s
                 ++ "\"" ++ " to \"" ++ fullPath dest ++ "\"?"
      cm <- showCopyModeChooserDialog
      withConfirmationDialog cmsg
        (runFileOp (FCopy . CC s dest $ cm)
         >> refreshTreeView mygui myview Nothing)
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
  mcdir <- getFirstRow myview
  rawModel'    <- readTVarIO $ rawModel myview
  sortedModel' <- readTVarIO $ sortedModel myview
  nv <- goUp =<< goUp mcdir
  refreshTreeView' mygui myview nv
