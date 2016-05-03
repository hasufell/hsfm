{-# LANGUAGE OverloadedStrings #-}

module FileSystem.FileOperations.CreateRegularFileSpec where


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


createRegularFileSpec :: Spec
createRegularFileSpec =
  describe "HSFM.FileSystem.FileOperations.createRegularFile" $ do

    -- successes --
    it "createRegularFile, all fine" $ do
      createRegularFile' "test/FileSystem/FileOperations/createRegularFileSpec/newDir"
      removeFileIfExists "test/FileSystem/FileOperations/createRegularFileSpec/newDir"

    -- posix failures --
    it "createRegularFile, can't write to destination directory" $
      createRegularFile' "test/FileSystem/FileOperations/createRegularFileSpec/noWritePerms/newDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "createRegularFile, can't write to destination directory" $
      createRegularFile' "test/FileSystem/FileOperations/createRegularFileSpec/noPerms/newDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "createRegularFile, destination file already exists" $
      createRegularFile' "test/FileSystem/FileOperations/createRegularFileSpec/alreadyExists"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

