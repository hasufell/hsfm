{-# LANGUAGE OverloadedStrings #-}

module FileSystem.FileOperations.RenameFileSpec where


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
      renameFile' "test/FileSystem/FileOperations/renameFileSpec/myFile"
                  "test/FileSystem/FileOperations/renameFileSpec/renamedFile"

    it "renameFile, all fine" $
      renameFile' "test/FileSystem/FileOperations/renameFileSpec/myFile"
                  "test/FileSystem/FileOperations/renameFileSpec/dir/renamedFile"

    it "renameFile, all fine on symlink" $
      renameFile' "test/FileSystem/FileOperations/renameFileSpec/myFileL"
                  "test/FileSystem/FileOperations/renameFileSpec/renamedFile"

    it "renameFile, all fine on directory" $
      renameFile' "test/FileSystem/FileOperations/renameFileSpec/dir"
                  "test/FileSystem/FileOperations/renameFileSpec/renamedFile"

    -- posix failures --
    it "renameFile, source file does not exist" $
      renameFile' "test/FileSystem/FileOperations/renameFileSpec/fileDoesNotExist"
                  "test/FileSystem/FileOperations/renameFileSpec/renamedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "renameFile, can't write to output directory" $
      renameFile' "test/FileSystem/FileOperations/renameFileSpec/myFile"
                  "test/FileSystem/FileOperations/renameFileSpec/noWritePerm/renamedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "renameFile, can't open output directory" $
      renameFile' "test/FileSystem/FileOperations/renameFileSpec/myFile"
                  "test/FileSystem/FileOperations/renameFileSpec/noPerms/renamedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "renameFile, can't open source directory" $
      renameFile' "test/FileSystem/FileOperations/renameFileSpec/noPerms/myFile"
                  "test/FileSystem/FileOperations/renameFileSpec/renamedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    -- custom failures --
    it "renameFile, destination file already exists" $
      renameFile' "test/FileSystem/FileOperations/renameFileSpec/myFile"
                  "test/FileSystem/FileOperations/renameFileSpec/alreadyExists"
        `shouldThrow`
        isFileDoesExist

    it "renameFile, move from file to dir" $
      renameFile' "test/FileSystem/FileOperations/renameFileSpec/myFile"
                  "test/FileSystem/FileOperations/renameFileSpec/alreadyExistsD"
        `shouldThrow`
        isDirDoesExist

    it "renameFile, source and dest are same file" $
      renameFile' "test/FileSystem/FileOperations/renameFileSpec/myFile"
                  "test/FileSystem/FileOperations/renameFileSpec/myFile"
        `shouldThrow`
        isSameFile

