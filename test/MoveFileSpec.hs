{-# LANGUAGE OverloadedStrings #-}

module MoveFileSpec where


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
      moveFile' "test/moveFileSpec/myFile"
                "test/moveFileSpec/movedFile"

    it "moveFile, all fine" $
      moveFile' "test/moveFileSpec/myFile"
                "test/moveFileSpec/dir/movedFile"

    it "moveFile, all fine on symlink" $
      moveFile' "test/moveFileSpec/myFileL"
                "test/moveFileSpec/movedFile"

    it "moveFile, all fine on directory" $
      moveFile' "test/moveFileSpec/dir"
                "test/moveFileSpec/movedFile"

    -- posix failures --
    it "moveFile, source file does not exist" $
      moveFile' "test/moveFileSpec/fileDoesNotExist"
                "test/moveFileSpec/movedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "moveFile, can't write to destination directory" $
      moveFile' "test/moveFileSpec/myFile"
                "test/moveFileSpec/noWritePerm/movedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "moveFile, can't open destination directory" $
      moveFile' "test/moveFileSpec/myFile"
                "test/moveFileSpec/noPerms/movedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "moveFile, can't open source directory" $
      moveFile' "test/moveFileSpec/noPerms/myFile"
                "test/moveFileSpec/movedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    -- custom failures --
    it "moveFile, destination file already exists" $
      moveFile' "test/moveFileSpec/myFile"
                "test/moveFileSpec/alreadyExists"
        `shouldThrow`
        isFileDoesExist

    it "moveFile, move from file to dir" $
      moveFile' "test/moveFileSpec/myFile"
                "test/moveFileSpec/alreadyExistsD"
        `shouldThrow`
        isDirDoesExist

    it "moveFile, source and dest are same file" $
      moveFile' "test/moveFileSpec/myFile"
                "test/moveFileSpec/myFile"
        `shouldThrow`
        isSameFile

