{-# LANGUAGE OverloadedStrings #-}

module FileSystem.FileOperations.GetFileTypeSpec where


import HSFM.FileSystem.FileOperations
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



getFileTypeSpec :: Spec
getFileTypeSpec =
  describe "HSFM.FileSystem.FileOperations.getFileType" $ do

    -- successes --
    it "getFileType, regular file" $
      getFileType' "test/FileSystem/FileOperations/getFileTypeSpec/regularfile"
        `shouldReturn` RegularFile

    it "getFileType, directory" $
      getFileType' "test/FileSystem/FileOperations/getFileTypeSpec/directory"
        `shouldReturn` Directory

    it "getFileType, directory with null permissions" $
      getFileType' "test/FileSystem/FileOperations/getFileTypeSpec/noPerms"
        `shouldReturn` Directory

    it "getFileType, symlink to file" $
      getFileType' "test/FileSystem/FileOperations/getFileTypeSpec/symlink"
        `shouldReturn` SymbolicLink

    it "getFileType, symlink to directory" $
      getFileType' "test/FileSystem/FileOperations/getFileTypeSpec/symlinkD"
        `shouldReturn` SymbolicLink

    it "getFileType, broken symlink" $
      getFileType' "test/FileSystem/FileOperations/getFileTypeSpec/brokenSymlink"
        `shouldReturn` SymbolicLink

    -- posix failures --
    it "getFileType, file does not exist" $
      getFileType' "test/FileSystem/FileOperations/getFileTypeSpec/nothingHere"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "getFileType, can't open directory" $
      getFileType' "test/FileSystem/FileOperations/getFileTypeSpec/noPerms/forz"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

