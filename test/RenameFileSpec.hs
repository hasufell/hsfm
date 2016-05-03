{-# LANGUAGE OverloadedStrings #-}

module RenameFileSpec where


import Test.Hspec
import HSFM.FileSystem.Errors
import System.IO.Error
  (
    ioeGetErrorType
  )
import GHC.IO.Exception
  (
    IOErrorType(..)
  )
import Utils


renameFileSpec :: Spec
renameFileSpec =
  describe "HSFM.FileSystem.FileOperations.renameFile" $ do

    -- successes --
    it "renameFile, all fine" $
      renameFile' "test/renameFileSpec/myFile"
                  "test/renameFileSpec/renamedFile"

    it "renameFile, all fine" $
      renameFile' "test/renameFileSpec/myFile"
                  "test/renameFileSpec/dir/renamedFile"

    it "renameFile, all fine on symlink" $
      renameFile' "test/renameFileSpec/myFileL"
                  "test/renameFileSpec/renamedFile"

    it "renameFile, all fine on directory" $
      renameFile' "test/renameFileSpec/dir"
                  "test/renameFileSpec/renamedFile"

    -- posix failures --
    it "renameFile, source file does not exist" $
      renameFile' "test/renameFileSpec/fileDoesNotExist"
                  "test/renameFileSpec/renamedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "renameFile, can't write to output directory" $
      renameFile' "test/renameFileSpec/myFile"
                  "test/renameFileSpec/noWritePerm/renamedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "renameFile, can't open output directory" $
      renameFile' "test/renameFileSpec/myFile"
                  "test/renameFileSpec/noPerms/renamedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "renameFile, can't open source directory" $
      renameFile' "test/renameFileSpec/noPerms/myFile"
                  "test/renameFileSpec/renamedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    -- custom failures --
    it "renameFile, destination file already exists" $
      renameFile' "test/renameFileSpec/myFile"
                  "test/renameFileSpec/alreadyExists"
        `shouldThrow`
        isFileDoesExist

    it "renameFile, move from file to dir" $
      renameFile' "test/renameFileSpec/myFile"
                  "test/renameFileSpec/alreadyExistsD"
        `shouldThrow`
        isDirDoesExist

    it "renameFile, source and dest are same file" $
      renameFile' "test/renameFileSpec/myFile"
                  "test/renameFileSpec/myFile"
        `shouldThrow`
        isSameFile

