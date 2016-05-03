{-# LANGUAGE OverloadedStrings #-}

module CreateDirSpec where


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


createDirSpec :: Spec
createDirSpec =
  describe "HSFM.FileSystem.FileOperations.createDir" $ do

    -- successes --
    it "createDir, all fine" $ do
      createDir' "test/createDirSpec/newDir"
      removeDirIfExists "test/createDirSpec/newDir"

    -- posix failures --
    it "createDir, can't write to output directory" $
      createDir' "test/createDirSpec/noWritePerms/newDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "createDir, can't open output directory" $
      createDir' "test/createDirSpec/noPerms/newDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "createDir, destination directory already exists" $
      createDir' "test/createDirSpec/alreadyExists"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)


