{-# LANGUAGE OverloadedStrings #-}

module FileSystem.FileOperations.CreateDirSpec where


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


spec :: Spec
spec =
  describe "HSFM.FileSystem.FileOperations.createDir" $ do

    -- successes --
    it "createDir, all fine" $ do
      createDir' "test/FileSystem/FileOperations/createDirSpec/newDir"
      removeDirIfExists "test/FileSystem/FileOperations/createDirSpec/newDir"

    -- posix failures --
    it "createDir, can't write to output directory" $
      createDir' "test/FileSystem/FileOperations/createDirSpec/noWritePerms/newDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "createDir, can't open output directory" $
      createDir' "test/FileSystem/FileOperations/createDirSpec/noPerms/newDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "createDir, destination directory already exists" $
      createDir' "test/FileSystem/FileOperations/createDirSpec/alreadyExists"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)


