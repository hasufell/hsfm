{-# LANGUAGE OverloadedStrings #-}

module FileSystem.FileOperations.DeleteDirSpec where


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
  describe "HSFM.FileSystem.FileOperations.deleteDir" $ do

    -- successes --
    it "deleteDir, empty directory, all fine" $ do
      createDir' "test/FileSystem/FileOperations/deleteDirSpec/testDir"
      deleteDir' "test/FileSystem/FileOperations/deleteDirSpec/testDir"
      getSymbolicLinkStatus "test/FileSystem/FileOperations/deleteDirSpec/testDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "deleteDir, directory with null permissions, all fine" $ do
      createDir' "test/FileSystem/FileOperations/deleteDirSpec/noPerms/testDir"
      noPerms "test/FileSystem/FileOperations/deleteDirSpec/noPerms/testDir"
      deleteDir' "test/FileSystem/FileOperations/deleteDirSpec/noPerms/testDir"
      getSymbolicLinkStatus "test/FileSystem/FileOperations/deleteDirSpec/testDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    -- posix failures --
    it "deleteDir, wrong file type (symlink to directory)" $
      deleteDir' "test/FileSystem/FileOperations/deleteDirSpec/dirSym"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "deleteDir, wrong file type (regular file)" $
      deleteDir' "test/FileSystem/FileOperations/deleteDirSpec/file"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "deleteDir, directory does not exist" $
      deleteDir' "test/FileSystem/FileOperations/deleteDirSpec/doesNotExist"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "deleteDir, directory not empty" $
      deleteDir' "test/FileSystem/FileOperations/deleteDirSpec/dir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == UnsatisfiedConstraints)

    it "deleteDir, can't open parent directory" $ do
      createDir' "test/FileSystem/FileOperations/deleteDirSpec/noPerms/foo"
      noPerms "test/FileSystem/FileOperations/deleteDirSpec/noPerms"
      (deleteDir' "test/FileSystem/FileOperations/deleteDirSpec/noPerms/foo"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied))
        >> normalDirPerms "test/FileSystem/FileOperations/deleteDirSpec/noPerms"
        >> deleteDir' "test/FileSystem/FileOperations/deleteDirSpec/noPerms/foo"

    it "deleteDir, can't write to parent directory, still fine" $ do
      createDir' "test/FileSystem/FileOperations/deleteDirSpec/noWritable/foo"
      noWritableDirPerms "test/FileSystem/FileOperations/deleteDirSpec/noWritable"
      (deleteDir' "test/FileSystem/FileOperations/deleteDirSpec/noWritable/foo"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied))
      normalDirPerms "test/FileSystem/FileOperations/deleteDirSpec/noWritable"
      deleteDir' "test/FileSystem/FileOperations/deleteDirSpec/noWritable/foo"



