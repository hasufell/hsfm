{-# OPTIONS_HADDOCK ignore-exports #-}

module GUI.Gtk.Dialogs where


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

