{-# LANGUAGE OverloadedStrings #-}

module FileSystem.FileOperations.MoveFileSpec where


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


moveFileSpec :: Spec
moveFileSpec =
  describe "HSFM.FileSystem.FileOperations.moveFile" $ do

    -- successes --
    it "moveFile, all fine" $
      moveFile' "test/FileSystem/FileOperations/moveFileSpec/myFile"
                "test/FileSystem/FileOperations/moveFileSpec/movedFile"

    it "moveFile, all fine" $
      moveFile' "test/FileSystem/FileOperations/moveFileSpec/myFile"
                "test/FileSystem/FileOperations/moveFileSpec/dir/movedFile"

    it "moveFile, all fine on symlink" $
      moveFile' "test/FileSystem/FileOperations/moveFileSpec/myFileL"
                "test/FileSystem/FileOperations/moveFileSpec/movedFile"

    it "moveFile, all fine on directory" $
      moveFile' "test/FileSystem/FileOperations/moveFileSpec/dir"
                "test/FileSystem/FileOperations/moveFileSpec/movedFile"

    -- posix failures --
    it "moveFile, source file does not exist" $
      moveFile' "test/FileSystem/FileOperations/moveFileSpec/fileDoesNotExist"
                "test/FileSystem/FileOperations/moveFileSpec/movedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "moveFile, can't write to destination directory" $
      moveFile' "test/FileSystem/FileOperations/moveFileSpec/myFile"
                "test/FileSystem/FileOperations/moveFileSpec/noWritePerm/movedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "moveFile, can't open destination directory" $
      moveFile' "test/FileSystem/FileOperations/moveFileSpec/myFile"
                "test/FileSystem/FileOperations/moveFileSpec/noPerms/movedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "moveFile, can't open source directory" $
      moveFile' "test/FileSystem/FileOperations/moveFileSpec/noPerms/myFile"
                "test/FileSystem/FileOperations/moveFileSpec/movedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    -- custom failures --
    it "moveFile, destination file already exists" $
      moveFile' "test/FileSystem/FileOperations/moveFileSpec/myFile"
                "test/FileSystem/FileOperations/moveFileSpec/alreadyExists"
        `shouldThrow`
        isFileDoesExist

    it "moveFile, move from file to dir" $
      moveFile' "test/FileSystem/FileOperations/moveFileSpec/myFile"
                "test/FileSystem/FileOperations/moveFileSpec/alreadyExistsD"
        `shouldThrow`
        isDirDoesExist

    it "moveFile, source and dest are same file" $
      moveFile' "test/FileSystem/FileOperations/moveFileSpec/myFile"
                "test/FileSystem/FileOperations/moveFileSpec/myFile"
        `shouldThrow`
        isSameFile

