{-# LANGUAGE OverloadedStrings #-}

module FileSystem.FileOperations.DeleteFileSpec where


import Test.Hspec
import System.IO.Error
  (
    ioeGetErrorType
  )
import System.Posix.Files.ByteString
  (
    getSymbolicLinkStatus
  )
import GHC.IO.Exception
  (
    IOErrorType(..)
  )
import Utils


spec :: Spec
spec =
  describe "HSFM.FileSystem.FileOperations.deleteFile" $ do

    -- successes --
    it "deleteFile, regular file, all fine" $ do
      createRegularFile' "test/FileSystem/FileOperations/deleteFileSpec/testFile"
      deleteFile' "test/FileSystem/FileOperations/deleteFileSpec/testFile"
      getSymbolicLinkStatus "test/FileSystem/FileOperations/deleteFileSpec/testFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "deleteFile, symlink, all fine" $ do
      recreateSymlink' "test/FileSystem/FileOperations/deleteFileSpec/syml"
                       "test/FileSystem/FileOperations/deleteFileSpec/testFile"
      deleteFile' "test/FileSystem/FileOperations/deleteFileSpec/testFile"
      getSymbolicLinkStatus "test/FileSystem/FileOperations/deleteFileSpec/testFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    -- posix failures --
    it "deleteFile, wrong file type (directory)" $
      deleteFile' "test/FileSystem/FileOperations/deleteFileSpec/dir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "deleteFile, file does not exist" $
      deleteFile' "test/FileSystem/FileOperations/deleteFileSpec/doesNotExist"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "deleteFile, can't read directory" $
      deleteFile' "test/FileSystem/FileOperations/deleteFileSpec/noPerms/blah"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

