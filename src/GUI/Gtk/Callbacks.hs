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
-- * 'fsState' reads
setCallbacks :: MyGUI -> MyView -> IO ()
setCallbacks mygui myview = do
  _ <- rootWin mygui `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "q"       <- fmap glibToString eventKeyName
    liftIO mainQuit
  _ <- treeView mygui `on` keyPressEvent $ tryEvent $ do
    [Control] <- eventModifier
    "h"       <- fmap glibToString eventKeyName
    liftIO $ modifyTVarIO (settings mygui)
                          (\x -> x { showHidden = not . showHidden $ x})
             >> (refreshTreeView' mygui myview =<< readTVarIO (fsState myview))
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
--
-- Interaction with mutable references:
--
-- * 'fsState' reads
open :: Row -> MyGUI -> MyView -> IO ()
open row mygui myview =
  case row of
    r@(_ :/ Dir _ _) -> do
      nv <- Data.DirTree.readFile $ fullPath r
      refreshTreeView' mygui myview nv
    r@(_ :/ RegFile _ _) ->
      withErrorDialog $ openFile $ fullPath r
    _ -> return ()


-- |Supposed to be used with 'withRow'. Deletes a file or directory.
--
-- Interaction with mutable references:
--
-- * 'fsState' reads
del :: Row -> MyGUI -> MyView -> IO ()
del row mygui myview =
  case row of
    r@(_ :/ Dir _ _)   -> do
      let fp = fullPath r
      subADT <- readDirectory fp
      let cmsg  = "Really delete directory \"" ++ fp ++ "\"?"
          cmsg2 = "Directory \"" ++ fp ++
                  "\" is not empty! Delete all contents?"
      withConfirmationDialog cmsg $
        if null subADT
          then withErrorDialog (deleteDir fp
                                >> refreshTreeView mygui myview Nothing)
          else withConfirmationDialog cmsg2 $ withErrorDialog
                 (deleteDirRecursive fp
                  >> refreshTreeView mygui myview Nothing)
    r@(_ :/ RegFile _ _) -> do
      let fp   = fullPath r
          cmsg = "Really delete file \"" ++ fp ++ "\"?"
      withConfirmationDialog cmsg
        $ withErrorDialog (deleteFile fp
                            >> refreshTreeView mygui myview Nothing)


-- |Supposed to be used with 'withRow'. Initializes a file copy operation.
--
-- Interaction with mutable references:
--
-- * 'operationBuffer' writes
-- * 'fsState' reads
copyInit :: Row -> MyGUI -> MyView -> IO ()
copyInit row mygui myview =
  writeTVarIO (operationBuffer myview) (FCopy . CP1 $ fullPath row)


-- |Finalizes a file copy operation.
--
-- Interaction with mutable references:
--
-- * 'operationBuffer' reads
-- * 'fsState' reads
copyFinal :: MyGUI -> MyView -> IO ()
copyFinal mygui myview = do
  op <- readTVarIO (operationBuffer myview)
  case op of
    FCopy (CP1 source) -> do
      dest   <- fullPath <$> readTVarIO (fsState myview)
      isFile <- doesFileExist source
      let cmsg = "Really copy file \"" ++ source
                 ++ "\"" ++ " to \"" ++ dest ++ "\"?"
      withConfirmationDialog cmsg $ do
        copyMode <- if isFile then return Strict else showCopyModeChooserDialog
        withErrorDialog ((runFileOp . FCopy . CC source dest $ copyMode)
                         >> refreshTreeView mygui myview Nothing)
    _ -> return ()


-- |Go up one directory and visualize it in the treeView.
--
-- Interaction with mutable references:
--
-- * 'rawModel' reads
-- * 'sortedModel' reads
-- * 'fsState' reads
upDir :: MyGUI -> MyView -> IO ()
upDir mygui myview = do
  rawModel'    <- readTVarIO $ rawModel myview
  sortedModel' <- readTVarIO $ sortedModel myview
  fS           <- readTVarIO $ fsState myview
  nv <- goUp fS
  refreshTreeView' mygui myview nv
