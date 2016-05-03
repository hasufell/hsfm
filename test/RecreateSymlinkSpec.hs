{-# LANGUAGE OverloadedStrings #-}

module RecreateSymlinkSpec where


import Test.Hspec
import System.IO.Error
  (
    ioeGetErrorType
  )
import GHC.IO.Exception
  (
    IOErrorType(..)
  )
import Utils


recreateSymlinkSpec :: Spec
recreateSymlinkSpec =
  describe "HSFM.FileSystem.FileOperations.recreateSymlink" $ do

    -- successes --
    it "recreateSymLink, all fine" $ do
      recreateSymlink' "test/recreateSymlinkSpec/myFileL"
                       "test/recreateSymlinkSpec/movedFile"
      removeFileIfExists "test/recreateSymlinkSpec/movedFile"

    it "recreateSymLink, all fine" $ do
      recreateSymlink' "test/recreateSymlinkSpec/myFileL"
                       "test/recreateSymlinkSpec/dir/movedFile"
      removeFileIfExists "test/recreateSymlinkSpec/dir/movedFile"

    -- posix failures --
    it "recreateSymLink, wrong input type (file)" $
      recreateSymlink' "test/recreateSymlinkSpec/myFile"
                       "test/recreateSymlinkSpec/movedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    it "recreateSymLink, wrong input type (directory)" $
      recreateSymlink' "test/recreateSymlinkSpec/dir"
                       "test/recreateSymlinkSpec/movedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    it "recreateSymLink, can't write to destination directory" $
      recreateSymlink' "test/recreateSymlinkSpec/myFileL"
                       "test/recreateSymlinkSpec/noWritePerm/movedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "recreateSymLink, can't open destination directory" $
      recreateSymlink' "test/recreateSymlinkSpec/myFileL"
                       "test/recreateSymlinkSpec/noPerms/movedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "recreateSymLink, can't open source directory" $
      recreateSymlink' "test/recreateSymlinkSpec/noPerms/myFileL"
                       "test/recreateSymlinkSpec/movedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "recreateSymLink, destination file already exists" $
      recreateSymlink' "test/recreateSymlinkSpec/myFileL"
                       "test/recreateSymlinkSpec/alreadyExists"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    it "recreateSymLink, destination already exists and is a dir" $
      recreateSymlink' "test/recreateSymlinkSpec/myFileL"
                       "test/recreateSymlinkSpec/alreadyExistsD"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    it "recreateSymLink, source and destination are the same file" $
      recreateSymlink' "test/recreateSymlinkSpec/myFileL"
                       "test/recreateSymlinkSpec/myFileL"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

