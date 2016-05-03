{-# LANGUAGE OverloadedStrings #-}

module DeleteDirSpec where


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


deleteDirSpec :: Spec
deleteDirSpec =
  describe "HSFM.FileSystem.FileOperations.deleteDir" $ do

    -- successes --
    it "deleteDir, empty directory, all fine" $ do
      createDir' "test/deleteDirSpec/testDir"
      deleteDir' "test/deleteDirSpec/testDir"
      getSymbolicLinkStatus "test/deleteDirSpec/testDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "deleteDir, directory with null permissions, all fine" $ do
      createDir' "test/deleteDirSpec/noPerms/testDir"
      noPerms "test/deleteDirSpec/noPerms/testDir"
      deleteDir' "test/deleteDirSpec/noPerms/testDir"
      getSymbolicLinkStatus "test/deleteDirSpec/testDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    -- posix failures --
    it "deleteDir, wrong file type (symlink to directory)" $
      deleteDir' "test/deleteDirSpec/dirSym"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "deleteDir, wrong file type (regular file)" $
      deleteDir' "test/deleteDirSpec/file"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "deleteDir, directory does not exist" $
      deleteDir' "test/deleteDirSpec/doesNotExist"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "deleteDir, directory not empty" $
      deleteDir' "test/deleteDirSpec/dir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == UnsatisfiedConstraints)

    it "deleteDir, can't open parent directory" $ do
      createDir' "test/deleteDirSpec/noPerms/foo"
      noPerms "test/deleteDirSpec/noPerms"
      (deleteDir' "test/deleteDirSpec/noPerms/foo"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied))
        >> normalDirPerms "test/deleteDirSpec/noPerms"
        >> deleteDir' "test/deleteDirSpec/noPerms/foo"

    it "deleteDir, can't write to parent directory, still fine" $ do
      createDir' "test/deleteDirSpec/noWritable/foo"
      noWritableDirPerms "test/deleteDirSpec/noWritable"
      (deleteDir' "test/deleteDirSpec/noWritable/foo"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied))
      normalDirPerms "test/deleteDirSpec/noWritable"
      deleteDir' "test/deleteDirSpec/noWritable/foo"



