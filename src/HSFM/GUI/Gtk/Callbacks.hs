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
{-# OPTIONS_HADDOCK ignore-exports #-}

module HSFM.GUI.Gtk.Callbacks where


import Control.Concurrent.STM
  (
    readTVarIO
  )
import Control.Exception
  (
    throwIO
  )
import Control.Monad
  (
    forM
  , forM_
  , join
  , void
  , when
  )
import Control.Monad.IO.Class
  (
    liftIO
  )
import Control.Monad.Loops
  (
    iterateUntil
  )
import Data.ByteString
  (
    ByteString
  )
import Data.ByteString.UTF8
  (
    fromString
  , toString
  )
import Data.Foldable
  (
    for_
  )
import Graphics.UI.Gtk
import qualified HPath as P
import HPath
  (
    fromAbs
  , Abs
  , Path
  )
import HPath.IO
import HPath.IO.Errors
import HPath.IO.Utils
import HSFM.FileSystem.FileType
import HSFM.FileSystem.UtilTypes
import HSFM.GUI.Gtk.Callbacks.Utils
import HSFM.GUI.Gtk.Data
import HSFM.GUI.Gtk.Dialogs
import HSFM.GUI.Gtk.MyView
import HSFM.GUI.Gtk.Settings
import HSFM.GUI.Gtk.Utils
import HSFM.History
import HSFM.Settings
import HSFM.Utils.IO
import Prelude hiding(readFile)
import System.Glib.UTFString
  (
    glibToString
  )
import qualified System.Posix.Process.ByteString as SPP
import System.Posix.Types
  (
    ProcessID
  )
import Control.Concurrent.MVar
  (
    putMVar
  , readMVar
  , takeMVar
  )





    -----------------
    --[ Callbacks ]--
    -----------------




---- MAIN CALLBACK ENTRYPOINT ----


-- |Set callbacks for the whole gui, on hotkeys, events and stuff.
setGUICallbacks :: MyGUI -> IO ()
setGUICallbacks mygui = do

  _ <- clearStatusBar mygui `on` buttonActivated $ do
       popStatusbar mygui
       writeTVarIO (operationBuffer mygui) None

  -- menubar-file
  _ <- (menubarFileQuit . menubar) mygui `on` menuItemActivated $
    mainQuit

  -- menubar-help
  _ <- (menubarHelpAbout . menubar) mygui `on` menuItemActivated $
    liftIO showAboutDialog
  return ()

  -- key events
  _ <- rootWin mygui `on` keyPressEvent $ tryEvent $ do
    QuitModifier <- eventModifier
    QuitKey      <- fmap glibToString eventKeyName
    liftIO mainQuit

  return ()


-- |Set callbacks specific to a given view, on hotkeys, events and stuff.
setViewCallbacks :: MyGUI -> MyView -> IO ()
setViewCallbacks mygui myview = do
  view' <- readTVarIO $ view myview
  case view' of
    fmv@(FMTreeView treeView) -> do
      _ <- treeView `on` rowActivated
             $ (\_ _ -> withItems mygui myview open)

      -- drag events
      _ <- treeView `on` dragBegin $
        \_ -> withItems mygui myview moveInit
      _ <- treeView `on` dragDrop $
         \dc p ts -> do
           p'    <- treeViewConvertWidgetToTreeCoords treeView p
           mpath <- treeViewGetPathAtPos treeView p'
           case mpath of
             Nothing -> do
               dragFinish dc False False ts
               return False
             Just _  -> do
               atom  <- atomNew ("HSFM" :: String)
               dragGetData treeView dc atom ts
               return True
      _ <- treeView `on` dragDataReceived $
        \dc p _ ts ->
          liftIO $ do
            signalStopEmission treeView "drag_data_received"
            p'    <- treeViewConvertWidgetToTreeCoords treeView p
            mpath <- treeViewGetPathAtPos treeView p'
            case mpath of
              Nothing         -> dragFinish dc False False ts
              Just (tp, _, _) -> do
                mitem <- rawPathToItem myview tp
                forM_ mitem $ \item ->
                  operationFinal mygui myview (Just item)
                dragFinish dc True False ts

      commonGuiEvents fmv
      return ()
    fmv@(FMIconView iconView) -> do
      _ <- iconView `on` itemActivated
             $ (\_ -> withItems mygui myview open)
      commonGuiEvents fmv
      return ()
  where
    commonGuiEvents fmv = do
      let view = fmViewToContainer fmv

      -- GUI events
      _ <- backViewB myview `on` buttonPressEvent $ do
        eb <- eventButton
        t  <- eventTime
        case eb of
          LeftButton  -> do
            liftIO $ void $ goHistoryBack mygui myview
            return True
          RightButton -> do
            his <- liftIO $ readMVar (history myview)
            menu <- liftIO $ mkHistoryMenuB mygui myview
                               (backwardsHistory his)
            _ <- liftIO $ menuPopup menu $ Just (RightButton, t)
            return True
          _           -> return False
      _ <- forwardViewB myview `on` buttonPressEvent $ do
        eb <- eventButton
        t  <- eventTime
        case eb of
          LeftButton  -> do
            liftIO $ void $ goHistoryForward mygui myview
            return True
          RightButton -> do
            his <- liftIO $ readMVar (history myview)
            menu <- liftIO $ mkHistoryMenuF mygui myview
                               (forwardHistory his)
            _ <- liftIO $ menuPopup menu $ Just (RightButton, t)
            return True
          _           -> return False
      _ <- urlBar myview `on` entryActivated $ urlGoTo mygui myview
      _ <- upViewB myview `on` buttonActivated $
           upDir mygui myview
      _ <- homeViewB myview `on` buttonActivated $
           goHome mygui myview
      _ <- refreshViewB myview `on` buttonActivated $ do
           cdir <- liftIO $ getCurrentDir myview
           refreshView mygui myview cdir

      -- key events
      _ <- viewBox myview `on` keyPressEvent $ tryEvent $ do
        ShowHiddenModifier <- eventModifier
        ShowHiddenKey      <- fmap glibToString eventKeyName
        cdir <- liftIO $ getCurrentDir myview
        liftIO $ modifyTVarIO (settings mygui)
                              (\x -> x { showHidden = not . showHidden $ x})
                 >> refreshView mygui myview cdir
      _ <- viewBox myview `on` keyPressEvent $ tryEvent $ do
        UpDirModifier <- eventModifier
        UpDirKey      <- fmap glibToString eventKeyName
        liftIO $ upDir mygui myview
      _ <- viewBox myview `on` keyPressEvent $ tryEvent $ do
        HistoryBackModifier <- eventModifier
        HistoryBackKey      <- fmap glibToString eventKeyName
        liftIO $ void $ goHistoryBack mygui myview
      _ <- viewBox myview `on` keyPressEvent $ tryEvent $ do
        HistoryForwardModifier <- eventModifier
        HistoryForwardKey      <- fmap glibToString eventKeyName
        liftIO $ void $ goHistoryForward mygui myview
      _ <- view `on` keyPressEvent $ tryEvent $ do
        DeleteModifier <- eventModifier
        DeleteKey      <- fmap glibToString eventKeyName
        liftIO $ withItems mygui myview del
      _ <- view `on` keyPressEvent $ tryEvent $ do
        OpenModifier <- eventModifier
        OpenKey      <- fmap glibToString eventKeyName
        liftIO $ withItems mygui myview open
      _ <- view `on` keyPressEvent $ tryEvent $ do
        CopyModifier <- eventModifier
        CopyKey      <- fmap glibToString eventKeyName
        liftIO $ withItems mygui myview copyInit
      _ <- view `on` keyPressEvent $ tryEvent $ do
        MoveModifier <- eventModifier
        MoveKey      <- fmap glibToString eventKeyName
        liftIO $ withItems mygui myview moveInit
      _ <- viewBox myview `on` keyPressEvent $ tryEvent $ do
        PasteModifier <- eventModifier
        PasteKey      <- fmap glibToString eventKeyName
        liftIO $ operationFinal mygui myview Nothing
      _ <- viewBox myview `on` keyPressEvent $ tryEvent $ do
        NewTabModifier <- eventModifier
        NewTabKey      <- fmap glibToString eventKeyName
        liftIO $ void $ newTab' mygui myview
      _ <- viewBox myview `on` keyPressEvent $ tryEvent $ do
        CloseTabModifier <- eventModifier
        CloseTabKey      <- fmap glibToString eventKeyName
        liftIO $ void $ closeTab mygui myview
      _ <- viewBox myview `on` keyPressEvent $ tryEvent $ do
        OpenTerminalModifier <- eventModifier
        OpenTerminalKey      <- fmap glibToString eventKeyName
        liftIO $ void $ openTerminalHere myview

      -- mouse button click
      _ <- view `on` buttonPressEvent $ do
        eb <- eventButton
        t  <- eventTime
        case eb of
          RightButton -> do
              _ <- liftIO $ menuPopup (rcMenu . rcmenu $ myview)
                          $ Just (RightButton, t)
              -- this is just to not screw with current selection
              -- on right-click
              -- TODO: this misbehaves under IconView
              (x, y) <- eventCoordinates
              mpath  <- liftIO $ getPathAtPos fmv (x, y)
              case mpath of
                -- item under the cursor, only pass on the signal
                -- if the item under the cursor is not within the current
                -- selection
                (Just tp) -> do
                  selectedTps <- liftIO $ getSelectedTreePaths mygui myview
                  return $ elem tp selectedTps
                -- no item under the cursor, pass on the signal
                Nothing -> return False
          MiddleButton -> do
            (x, y) <- eventCoordinates
            mitem  <- liftIO $ (getPathAtPos fmv (x, y))
                               >>= \mpos -> fmap join
                                 $ forM mpos (rawPathToItem myview)

            case mitem of
              -- item under the cursor, only pass on the signal
              -- if the item under the cursor is not within the current
              -- selection
              (Just item) -> do
                liftIO $ opeInNewTab mygui item
                return True
              -- no item under the cursor, pass on the signal
              Nothing -> return False

          OtherButton 8 -> do
            liftIO $ void $ goHistoryBack mygui myview
            return False
          OtherButton 9 -> do
            liftIO $ void $ goHistoryForward mygui myview
            return False
          -- not right-click, so pass on the signal
          _ -> return False

      -- right click menu
      _ <- (rcFileOpen . rcmenu) myview `on` menuItemActivated $
        liftIO $ withItems mygui myview open
      _ <- (rcFileExecute . rcmenu) myview `on` menuItemActivated $
        liftIO $ withItems mygui myview execute
      _ <- (rcFileNewRegFile . rcmenu) myview `on` menuItemActivated $
        liftIO $ newFile mygui myview
      _ <- (rcFileNewDir . rcmenu) myview `on` menuItemActivated $
        liftIO $ newDir mygui myview
      _ <- (rcFileNewTab . rcmenu) myview `on` menuItemActivated $
        liftIO $ newTab' mygui myview
      _ <- (rcFileNewTerm . rcmenu) myview `on` menuItemActivated $
        liftIO $ void $ openTerminalHere myview
      _ <- (rcFileCopy . rcmenu) myview `on` menuItemActivated $
        liftIO $ withItems mygui myview copyInit
      _ <- (rcFileRename . rcmenu) myview `on` menuItemActivated $
        liftIO $ withItems mygui myview renameF
      _ <- (rcFilePaste . rcmenu) myview `on` menuItemActivated $
        liftIO $ operationFinal mygui myview Nothing
      _ <- (rcFileDelete . rcmenu) myview `on` menuItemActivated $
        liftIO $ withItems mygui myview del
      _ <- (rcFileProperty . rcmenu) myview `on` menuItemActivated $
        liftIO $ withItems mygui myview showFilePropertyDialog
      _ <- (rcFileCut . rcmenu) myview `on` menuItemActivated $
        liftIO $ withItems mygui myview moveInit
      _ <- (rcFileIconView . rcmenu) myview `on` menuItemActivated $
        liftIO $ switchView mygui myview createIconView
      _ <- (rcFileTreeView . rcmenu) myview `on` menuItemActivated $
        liftIO $ switchView mygui myview createTreeView
      return ()

    getPathAtPos fmv (x, y) =
      case fmv of
        FMTreeView treeView -> do
          mp <- treeViewGetPathAtPos treeView (round x, round y)
          return $ fmap (\(p, _, _) -> p) mp
        FMIconView iconView ->
           fmap (\tp -> if null tp then Nothing else Just tp)
                  $ iconViewGetPathAtPos iconView (round x) (round y)




---- OTHER ----


openTerminalHere :: MyView -> IO ProcessID
openTerminalHere myview = do
  cwd <- (P.fromAbs . path) <$> getCurrentDir myview
  SPP.forkProcess $ terminalCommand cwd




---- TAB OPERATIONS ----


-- |Closes the current tab, but only if there is more than one tab.
closeTab :: MyGUI -> MyView -> IO ()
closeTab mygui myview = do
  n <- notebookGetNPages (notebook mygui)
  when (n > 1) $ void $ destroyView mygui myview


newTab' :: MyGUI -> MyView -> IO ()
newTab' mygui myview = do
  cwd <- getCurrentDir myview
  void $ withErrorDialog $ newTab mygui createTreeView cwd (-1)


opeInNewTab :: MyGUI -> Item -> IO ()
opeInNewTab mygui item@(DirOrSym _) =
  void $ withErrorDialog $ newTab mygui createTreeView item (-1)
opeInNewTab _ _ = return ()



---- FILE OPERATION CALLBACKS (COPY, MOVE, ...) ----


-- |Supposed to be used with 'withRows'. Deletes a file or directory.
del :: [Item] -> MyGUI -> MyView -> IO ()
del [item] _ _ = withErrorDialog $ do
  let cmsg  = "Really delete \"" ++ getFPasStr item ++ "\"?"
  withConfirmationDialog cmsg
    $ easyDelete . path $ item
-- this throws on the first error that occurs
del items@(_:_) _ _ = withErrorDialog $ do
  let cmsg  = "Really delete " ++ show (length items) ++ " files?"
  withConfirmationDialog cmsg
    $ forM_ items $ \item -> easyDelete . path $ item
del _ _ _ = withErrorDialog
              . throwIO $ InvalidOperation
                          "Operation not supported on multiple files"


-- |Initializes a file move operation.
moveInit :: [Item] -> MyGUI -> MyView -> IO ()
moveInit items@(_:_) mygui _ = do
  writeTVarIO (operationBuffer mygui) (FMove . PartialMove . map path $ items)
  let sbmsg = case items of
              (item:[]) -> "Move buffer: " ++ getFPasStr item
              _         -> "Move buffer: " ++ (show . length $ items)
                                           ++ " items"
  popStatusbar mygui
  void $ pushStatusBar mygui sbmsg
moveInit _ _ _ = withErrorDialog
                   . throwIO $ InvalidOperation
                               "No file selected!"

-- |Supposed to be used with 'withRows'. Initializes a file copy operation.
copyInit :: [Item] -> MyGUI -> MyView -> IO ()
copyInit items@(_:_) mygui _ = do
  writeTVarIO (operationBuffer mygui) (FCopy . PartialCopy . map path $ items)
  let sbmsg = case items of
              (item:[]) -> "Copy buffer: " ++ getFPasStr item
              _         -> "Copy buffer: " ++ (show . length $ items)
                                           ++ " items"
  popStatusbar mygui
  void $ pushStatusBar mygui sbmsg
copyInit _ _ _ = withErrorDialog
                   . throwIO $ InvalidOperation
                               "No file selected!"


-- |Finalizes a file operation, such as copy or move.
operationFinal :: MyGUI -> MyView -> Maybe Item -> IO ()
operationFinal mygui myview mitem = withErrorDialog $ do
  op <- readTVarIO (operationBuffer mygui)
  cdir <- case mitem of
            Nothing -> path <$> getCurrentDir myview
            Just x  -> return $ path x
  case op of
    FMove (PartialMove s) -> do
      let cmsg = "Really move " ++ imsg s
                  ++ " to \"" ++ toString (P.fromAbs cdir)
                  ++ "\"?"
      withConfirmationDialog cmsg $ doFileOperation (FMove $ Move s cdir)
      popStatusbar mygui
      writeTVarIO (operationBuffer mygui) None
    FCopy (PartialCopy s) -> do
      let cmsg = "Really copy " ++ imsg s
                 ++ " to \"" ++ toString (P.fromAbs cdir)
                 ++ "\"?"
      withConfirmationDialog cmsg $ doFileOperation (FCopy $ Copy s cdir)
    _ -> return ()
  where
    imsg s = case s of
               (item:[]) -> "\"" ++ toString (P.fromAbs item) ++ "\""
               items     -> (show . length $ items) ++ " items"


-- |Create a new file.
newFile :: MyGUI -> MyView -> IO ()
newFile _ myview = withErrorDialog $ do
  mfn   <- textInputDialog "Enter file name" ("" :: String)
  let pmfn = P.parseFn =<< fromString <$> mfn
  for_ pmfn $ \fn -> do
    cdir  <- getCurrentDir myview
    createRegularFile newFilePerms (path cdir P.</> fn)


-- |Create a new directory.
newDir :: MyGUI -> MyView -> IO ()
newDir _ myview = withErrorDialog $ do
  mfn   <- textInputDialog "Enter directory name" ("" :: String)
  let pmfn = P.parseFn =<< fromString <$> mfn
  for_ pmfn $ \fn -> do
    cdir  <- getCurrentDir myview
    createDir newDirPerms (path cdir P.</> fn)


renameF :: [Item] -> MyGUI -> MyView -> IO ()
renameF [item] _ _ = withErrorDialog $ do
  iname <- P.fromRel <$> (P.basename $ path item)
  mfn  <- textInputDialog "Enter new file name" (iname :: ByteString)
  let pmfn = P.parseFn =<< fromString <$> mfn
  for_ pmfn $ \fn -> do
    let cmsg = "Really rename \"" ++ getFPasStr item
               ++ "\"" ++ " to \""
               ++ toString (P.fromAbs $ (P.dirname . path $ item)
                                             P.</> fn) ++ "\"?"
    withConfirmationDialog cmsg $
      HPath.IO.renameFile (path item)
                          ((P.dirname $ path item) P.</> fn)
renameF _ _ _ = withErrorDialog
                  . throwIO $ InvalidOperation
                              "Operation not supported on multiple files"




---- DIRECTORY TRAVERSAL AND FILE OPENING CALLBACKS ----


-- |Go to the url given at the 'urlBar' and visualize it in the given
-- treeView.
--
-- If the url is invalid, does nothing.
urlGoTo :: MyGUI -> MyView -> IO ()
urlGoTo mygui myview = withErrorDialog $ do
  fp <- entryGetText (urlBar myview)
  forM_ (P.parseAbs fp :: Maybe (Path Abs)) $ \fp' ->
      whenM (canOpenDirectory fp')
            (goDir True mygui myview =<< (readFile getFileInfo $ fp'))


goHome :: MyGUI -> MyView -> IO ()
goHome mygui myview =  withErrorDialog $ do
  homedir <- home
  forM_ (P.parseAbs homedir :: Maybe (Path Abs)) $ \fp' ->
      whenM (canOpenDirectory fp')
            (goDir True mygui myview =<< (readFile getFileInfo $ fp'))


-- |Execute a given file.
execute :: [Item] -> MyGUI -> MyView -> IO ()
execute [item] _ _ = withErrorDialog $
  void $ executeFile (path item) []
execute _ _ _ = withErrorDialog
                  . throwIO $ InvalidOperation
                              "Operation not supported on multiple files"


-- |Supposed to be used with 'withRows'. Opens a file or directory.
open :: [Item] -> MyGUI -> MyView -> IO ()
open [item] mygui myview = withErrorDialog $
  case item of
    DirOrSym r -> do
      nv <- readFile getFileInfo $ path r
      goDir True mygui myview nv
    r ->
      void $ openFile . path $ r
open items mygui _ = do
  let dirs  = filter (fst . sdir) items
      files = filter (fst . sfileLike) items
  forM_ dirs (withErrorDialog . opeInNewTab mygui)
  forM_ files (withErrorDialog . openFile . path)


-- |Go up one directory and visualize it in the treeView.
upDir :: MyGUI -> MyView -> IO ()
upDir mygui myview = withErrorDialog $ do
  cdir <- getCurrentDir myview
  nv <- goUp cdir
  goDir True mygui myview nv




---- HISTORY CALLBACKS ----


-- |Go "back" in the history.
goHistoryBack :: MyGUI -> MyView -> IO (Path Abs)
goHistoryBack mygui myview = do
  hs <- takeMVar (history myview)
  let nhs = historyBack hs
  putMVar (history myview) nhs
  nv <- readFile getFileInfo $ currentDir nhs
  goDir False mygui myview nv
  return $ currentDir nhs


-- |Go "forward" in the history.
goHistoryForward :: MyGUI -> MyView -> IO (Path Abs)
goHistoryForward mygui myview = do
  hs <- takeMVar (history myview)
  let nhs = historyForward hs
  putMVar (history myview) nhs
  nv <- readFile getFileInfo $ currentDir nhs
  goDir False mygui myview nv
  return $ currentDir nhs


-- |Show backwards history in a drop-down menu, depending on the input.
mkHistoryMenuB :: MyGUI -> MyView -> [Path Abs] -> IO Menu
mkHistoryMenuB mygui myview hs = do
  menu <- menuNew
  menuitems <- forM hs $ \p -> do
    item <- menuItemNewWithLabel (fromAbs p)
    _ <- item `on` menuItemActivated $
      void $ iterateUntil (== p) (goHistoryBack mygui myview)
    return item
  forM_ menuitems $ \item -> menuShellAppend menu item
  widgetShowAll menu
  return menu


-- |Show forward history in a drop-down menu, depending on the input.
mkHistoryMenuF :: MyGUI -> MyView -> [Path Abs] -> IO Menu
mkHistoryMenuF mygui myview hs = do
  menu <- menuNew
  menuitems <- forM hs $ \p -> do
    item <- menuItemNewWithLabel (fromAbs p)
    _ <- item `on` menuItemActivated $
      void $ iterateUntil (== p) (goHistoryForward mygui myview)
    return item
  forM_ menuitems $ \item -> menuShellAppend menu item
  widgetShowAll menu
  return menu

