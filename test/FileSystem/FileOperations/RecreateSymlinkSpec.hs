{-# LANGUAGE OverloadedStrings #-}

module FileSystem.FileOperations.RecreateSymlinkSpec where


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
      recreateSymlink' "test/FileSystem/FileOperations/recreateSymlinkSpec/myFileL"
                       "test/FileSystem/FileOperations/recreateSymlinkSpec/movedFile"
      removeFileIfExists "test/FileSystem/FileOperations/recreateSymlinkSpec/movedFile"

    it "recreateSymLink, all fine" $ do
      recreateSymlink' "test/FileSystem/FileOperations/recreateSymlinkSpec/myFileL"
                       "test/FileSystem/FileOperations/recreateSymlinkSpec/dir/movedFile"
      removeFileIfExists "test/FileSystem/FileOperations/recreateSymlinkSpec/dir/movedFile"

    -- posix failures --
    it "recreateSymLink, wrong input type (file)" $
      recreateSymlink' "test/FileSystem/FileOperations/recreateSymlinkSpec/myFile"
                       "test/FileSystem/FileOperations/recreateSymlinkSpec/movedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    it "recreateSymLink, wrong input type (directory)" $
      recreateSymlink' "test/FileSystem/FileOperations/recreateSymlinkSpec/dir"
                       "test/FileSystem/FileOperations/recreateSymlinkSpec/movedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    it "recreateSymLink, can't write to destination directory" $
      recreateSymlink' "test/FileSystem/FileOperations/recreateSymlinkSpec/myFileL"
                       "test/FileSystem/FileOperations/recreateSymlinkSpec/noWritePerm/movedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "recreateSymLink, can't open destination directory" $
      recreateSymlink' "test/FileSystem/FileOperations/recreateSymlinkSpec/myFileL"
                       "test/FileSystem/FileOperations/recreateSymlinkSpec/noPerms/movedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "recreateSymLink, can't open source directory" $
      recreateSymlink' "test/FileSystem/FileOperations/recreateSymlinkSpec/noPerms/myFileL"
                       "test/FileSystem/FileOperations/recreateSymlinkSpec/movedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "recreateSymLink, destination file already exists" $
      recreateSymlink' "test/FileSystem/FileOperations/recreateSymlinkSpec/myFileL"
                       "test/FileSystem/FileOperations/recreateSymlinkSpec/alreadyExists"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    it "recreateSymLink, destination already exists and is a dir" $
      recreateSymlink' "test/FileSystem/FileOperations/recreateSymlinkSpec/myFileL"
                       "test/FileSystem/FileOperations/recreateSymlinkSpec/alreadyExistsD"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    it "recreateSymLink, source and destination are the same file" $
      recreateSymlink' "test/FileSystem/FileOperations/recreateSymlinkSpec/myFileL"
                       "test/FileSystem/FileOperations/recreateSymlinkSpec/myFileL"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

