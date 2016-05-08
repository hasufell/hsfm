{-# LANGUAGE OverloadedStrings #-}

module FileSystem.FileOperations.DeleteDirRecursiveSpec where


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
  describe "HSFM.FileSystem.FileOperations.deleteDirRecursive" $ do

    -- successes --
    it "deleteDirRecursive, empty directory, all fine" $ do
      createDir' "test/FileSystem/FileOperations/deleteDirRecursiveSpec/testDir"
      deleteDirRecursive' "test/FileSystem/FileOperations/deleteDirRecursiveSpec/testDir"
      getSymbolicLinkStatus "test/FileSystem/FileOperations/deleteDirRecursiveSpec/testDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "deleteDirRecursive, empty directory with null permissions, all fine" $ do
      createDir' "test/FileSystem/FileOperations/deleteDirRecursiveSpec/noPerms/testDir"
      noPerms "test/FileSystem/FileOperations/deleteDirRecursiveSpec/noPerms/testDir"
      deleteDirRecursive' "test/FileSystem/FileOperations/deleteDirRecursiveSpec/noPerms/testDir"

    it "deleteDirRecursive, non-empty directory, all fine" $ do
      createDir' "test/FileSystem/FileOperations/deleteDirRecursiveSpec/nonEmpty"
      createDir' "test/FileSystem/FileOperations/deleteDirRecursiveSpec/nonEmpty/dir1"
      createDir' "test/FileSystem/FileOperations/deleteDirRecursiveSpec/nonEmpty/dir2"
      createDir' "test/FileSystem/FileOperations/deleteDirRecursiveSpec/nonEmpty/dir2/dir3"
      createRegularFile' "test/FileSystem/FileOperations/deleteDirRecursiveSpec/nonEmpty/file1"
      createRegularFile' "test/FileSystem/FileOperations/deleteDirRecursiveSpec/nonEmpty/dir1/file2"
      deleteDirRecursive' "test/FileSystem/FileOperations/deleteDirRecursiveSpec/nonEmpty"
      getSymbolicLinkStatus "test/FileSystem/FileOperations/deleteDirRecursiveSpec/nonEmpty"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    -- posix failures --
    it "deleteDirRecursive, can't open parent directory" $ do
      createDir' "test/FileSystem/FileOperations/deleteDirRecursiveSpec/noPerms/foo"
      noPerms "test/FileSystem/FileOperations/deleteDirRecursiveSpec/noPerms"
      (deleteDirRecursive' "test/FileSystem/FileOperations/deleteDirRecursiveSpec/noPerms/foo"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied))
        >> normalDirPerms "test/FileSystem/FileOperations/deleteDirRecursiveSpec/noPerms"
        >> deleteDir' "test/FileSystem/FileOperations/deleteDirRecursiveSpec/noPerms/foo"

    it "deleteDirRecursive, can't write to parent directory" $ do
      createDir' "test/FileSystem/FileOperations/deleteDirRecursiveSpec/noWritable/foo"
      noWritableDirPerms "test/FileSystem/FileOperations/deleteDirRecursiveSpec/noWritable"
      (deleteDirRecursive' "test/FileSystem/FileOperations/deleteDirRecursiveSpec/noWritable/foo"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied))
      normalDirPerms "test/FileSystem/FileOperations/deleteDirRecursiveSpec/noWritable"
      deleteDir' "test/FileSystem/FileOperations/deleteDirRecursiveSpec/noWritable/foo"

    it "deleteDirRecursive, wrong file type (symlink to directory)" $
      deleteDirRecursive' "test/FileSystem/FileOperations/deleteDirRecursiveSpec/dirSym"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "deleteDirRecursive, wrong file type (regular file)" $
      deleteDirRecursive' "test/FileSystem/FileOperations/deleteDirRecursiveSpec/file"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "deleteDirRecursive, directory does not exist" $
      deleteDirRecursive' "test/FileSystem/FileOperations/deleteDirRecursiveSpec/doesNotExist"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)


