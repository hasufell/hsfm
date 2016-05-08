{-# LANGUAGE OverloadedStrings #-}

module FileSystem.FileOperations.MoveFileOverwriteSpec where


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


spec :: Spec
spec =
  describe "HSFM.FileSystem.FileOperations.moveFileOverwrite" $ do

    -- successes --
    it "moveFileOverwrite, all fine" $
      moveFileOverwrite' "test/FileSystem/FileOperations/moveFileOverwriteSpec/myFile"
                         "test/FileSystem/FileOperations/moveFileOverwriteSpec/movedFile"

    it "moveFileOverwrite, all fine" $
      moveFileOverwrite' "test/FileSystem/FileOperations/moveFileOverwriteSpec/myFile"
                         "test/FileSystem/FileOperations/moveFileOverwriteSpec/dir/movedFile"

    it "moveFileOverwrite, all fine on symlink" $
      moveFileOverwrite' "test/FileSystem/FileOperations/moveFileOverwriteSpec/myFileL"
                         "test/FileSystem/FileOperations/moveFileOverwriteSpec/movedFile"

    it "moveFileOverwrite, all fine on directory" $
      moveFileOverwrite' "test/FileSystem/FileOperations/moveFileOverwriteSpec/dir"
                         "test/FileSystem/FileOperations/moveFileOverwriteSpec/movedFile"

    it "moveFileOverwrite, destination file already exists" $
      moveFileOverwrite' "test/FileSystem/FileOperations/moveFileOverwriteSpec/myFile"
                         "test/FileSystem/FileOperations/moveFileOverwriteSpec/alreadyExists"

    -- posix failures --
    it "moveFileOverwrite, source file does not exist" $
      moveFileOverwrite' "test/FileSystem/FileOperations/moveFileOverwriteSpec/fileDoesNotExist"
                         "test/FileSystem/FileOperations/moveFileOverwriteSpec/movedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "moveFileOverwrite, can't write to destination directory" $
      moveFileOverwrite' "test/FileSystem/FileOperations/moveFileOverwriteSpec/myFile"
                         "test/FileSystem/FileOperations/moveFileOverwriteSpec/noWritePerm/movedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "moveFileOverwrite, can't open destination directory" $
      moveFileOverwrite' "test/FileSystem/FileOperations/moveFileOverwriteSpec/myFile"
                         "test/FileSystem/FileOperations/moveFileOverwriteSpec/noPerms/movedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "moveFileOverwrite, can't open source directory" $
      moveFileOverwrite' "test/FileSystem/FileOperations/moveFileOverwriteSpec/noPerms/myFile"
                         "test/FileSystem/FileOperations/moveFileOverwriteSpec/movedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    -- custom failures --
    it "moveFileOverwrite, move from file to dir" $
      moveFileOverwrite' "test/FileSystem/FileOperations/moveFileOverwriteSpec/myFile"
                         "test/FileSystem/FileOperations/moveFileOverwriteSpec/alreadyExistsD"
        `shouldThrow`
        isDirDoesExist

    it "moveFileOverwrite, source and dest are same file" $
      moveFileOverwrite' "test/FileSystem/FileOperations/moveFileOverwriteSpec/myFile"
                         "test/FileSystem/FileOperations/moveFileOverwriteSpec/myFile"
        `shouldThrow`
        isSameFile

