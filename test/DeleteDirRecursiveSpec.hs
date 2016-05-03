{-# LANGUAGE OverloadedStrings #-}

module DeleteDirRecursiveSpec where


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


deleteDirRecursiveSpec :: Spec
deleteDirRecursiveSpec =
  describe "HSFM.FileSystem.FileOperations.deleteDirRecursive" $ do

    -- successes --
    it "deleteDirRecursive, empty directory, all fine" $ do
      createDir' "test/deleteDirRecursiveSpec/testDir"
      deleteDirRecursive' "test/deleteDirRecursiveSpec/testDir"
      getSymbolicLinkStatus "test/deleteDirRecursiveSpec/testDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "deleteDirRecursive, empty directory with null permissions, all fine" $ do
      createDir' "test/deleteDirRecursiveSpec/noPerms/testDir"
      noPerms "test/deleteDirRecursiveSpec/noPerms/testDir"
      deleteDirRecursive' "test/deleteDirRecursiveSpec/noPerms/testDir"

    it "deleteDirRecursive, non-empty directory, all fine" $ do
      createDir' "test/deleteDirRecursiveSpec/nonEmpty"
      createDir' "test/deleteDirRecursiveSpec/nonEmpty/dir1"
      createDir' "test/deleteDirRecursiveSpec/nonEmpty/dir2"
      createDir' "test/deleteDirRecursiveSpec/nonEmpty/dir2/dir3"
      createRegularFile' "test/deleteDirRecursiveSpec/nonEmpty/file1"
      createRegularFile' "test/deleteDirRecursiveSpec/nonEmpty/dir1/file2"
      deleteDirRecursive' "test/deleteDirRecursiveSpec/nonEmpty"
      getSymbolicLinkStatus "test/deleteDirRecursiveSpec/nonEmpty"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    -- posix failures --
    it "deleteDirRecursive, can't open parent directory" $ do
      createDir' "test/deleteDirRecursiveSpec/noPerms/foo"
      noPerms "test/deleteDirRecursiveSpec/noPerms"
      (deleteDirRecursive' "test/deleteDirRecursiveSpec/noPerms/foo"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied))
        >> normalDirPerms "test/deleteDirRecursiveSpec/noPerms"
        >> deleteDir' "test/deleteDirRecursiveSpec/noPerms/foo"

    it "deleteDirRecursive, can't write to parent directory" $ do
      createDir' "test/deleteDirRecursiveSpec/noWritable/foo"
      noWritableDirPerms "test/deleteDirRecursiveSpec/noWritable"
      (deleteDirRecursive' "test/deleteDirRecursiveSpec/noWritable/foo"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied))
      normalDirPerms "test/deleteDirRecursiveSpec/noWritable"
      deleteDir' "test/deleteDirRecursiveSpec/noWritable/foo"

    it "deleteDirRecursive, wrong file type (symlink to directory)" $
      deleteDirRecursive' "test/deleteDirRecursiveSpec/dirSym"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "deleteDirRecursive, wrong file type (regular file)" $
      deleteDirRecursive' "test/deleteDirRecursiveSpec/file"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "deleteDirRecursive, directory does not exist" $
      deleteDirRecursive' "test/deleteDirRecursiveSpec/doesNotExist"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)


