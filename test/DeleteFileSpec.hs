{-# LANGUAGE OverloadedStrings #-}

module DeleteFileSpec where


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


deleteFileSpec :: Spec
deleteFileSpec =
  describe "HSFM.FileSystem.FileOperations.deleteFile" $ do

    -- successes --
    it "deleteFile, regular file, all fine" $ do
      createRegularFile' "test/deleteFileSpec/testFile"
      deleteFile' "test/deleteFileSpec/testFile"
      getSymbolicLinkStatus "test/deleteFileSpec/testFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "deleteFile, symlink, all fine" $ do
      recreateSymlink' "test/deleteFileSpec/syml"
                       "test/deleteFileSpec/testFile"
      deleteFile' "test/deleteFileSpec/testFile"
      getSymbolicLinkStatus "test/deleteFileSpec/testFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    -- posix failures --
    it "deleteFile, wrong file type (directory)" $
      deleteFile' "test/deleteFileSpec/dir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "deleteFile, file does not exist" $
      deleteFile' "test/deleteFileSpec/doesNotExist"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "deleteFile, can't read directory" $
      deleteFile' "test/deleteFileSpec/noPerms/blah"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

