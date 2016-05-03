{-# LANGUAGE OverloadedStrings #-}

module CreateRegularFileSpec where


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
      createRegularFile' "test/createRegularFileSpec/newDir"
      removeFileIfExists "test/createRegularFileSpec/newDir"

    -- posix failures --
    it "createRegularFile, can't write to destination directory" $
      createRegularFile' "test/createRegularFileSpec/noWritePerms/newDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "createRegularFile, can't write to destination directory" $
      createRegularFile' "test/createRegularFileSpec/noPerms/newDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "createRegularFile, destination file already exists" $
      createRegularFile' "test/createRegularFileSpec/alreadyExists"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

