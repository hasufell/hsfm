{-# LANGUAGE OverloadedStrings #-}

module FileSystem.FileOperations.CopyDirRecursiveSpec where


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
import System.Exit
import System.Process
import Utils



copyDirRecursiveSpec :: Spec
copyDirRecursiveSpec =
  describe "HSFM.FileSystem.FileOperations.copyDirRecursive" $ do

    -- successes --
    it "copyDirRecursive, all fine" $ do
      copyDirRecursive' "test/FileSystem/FileOperations/copyDirRecursiveSpec/inputDir"
                        "test/FileSystem/FileOperations/copyDirRecursiveSpec/outputDir"
      removeDirIfExists "test/FileSystem/FileOperations/copyDirRecursiveSpec/outputDir"

    it "copyDirRecursive, all fine and compare" $ do
      copyDirRecursive' "test/FileSystem/FileOperations/copyDirRecursiveSpec/inputDir"
                        "test/FileSystem/FileOperations/copyDirRecursiveSpec/outputDir"
      (system $ "diff -r --no-dereference "
                          ++ "test/FileSystem/FileOperations/copyDirRecursiveSpec/inputDir" ++ " "
                          ++ "test/FileSystem/FileOperations/copyDirRecursiveSpec/outputDir")
        `shouldReturn` ExitSuccess
      removeDirIfExists "test/FileSystem/FileOperations/copyDirRecursiveSpec/outputDir"

    -- posix failures --
    it "copyDirRecursive, source directory does not exist" $
      copyDirRecursive' "test/FileSystem/FileOperations/copyDirRecursiveSpec/doesNotExist"
                        "test/FileSystem/FileOperations/copyDirRecursiveSpec/outputDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "copyDirRecursive, no write permission on output dir" $
      copyDirRecursive' "test/FileSystem/FileOperations/copyDirRecursiveSpec/inputDir"
                        "test/FileSystem/FileOperations/copyDirRecursiveSpec/noWritePerm/foo"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyDirRecursive, cannot open output dir" $
      copyDirRecursive' "test/FileSystem/FileOperations/copyDirRecursiveSpec/inputDir"
                        "test/FileSystem/FileOperations/copyDirRecursiveSpec/noPerms/foo"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyDirRecursive, cannot open source dir" $
      copyDirRecursive' "test/FileSystem/FileOperations/copyDirRecursiveSpec/noPerms/inputDir"
                        "test/FileSystem/FileOperations/copyDirRecursiveSpec/foo"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyDirRecursive, destination dir already exists" $
      copyDirRecursive' "test/FileSystem/FileOperations/copyDirRecursiveSpec/inputDir"
                        "test/FileSystem/FileOperations/copyDirRecursiveSpec/alreadyExistsD"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    it "copyDirRecursive, destination already exists and is a file" $
      copyDirRecursive' "test/FileSystem/FileOperations/copyDirRecursiveSpec/inputDir"
                        "test/FileSystem/FileOperations/copyDirRecursiveSpec/alreadyExists"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    it "copyDirRecursive, destination and source same directory" $
      copyDirRecursive' "test/FileSystem/FileOperations/copyDirRecursiveSpec/inputDir"
                        "test/FileSystem/FileOperations/copyDirRecursiveSpec/inputDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    it "copyDirRecursive, wrong input (regular file)" $
      copyDirRecursive' "test/FileSystem/FileOperations/copyDirRecursiveSpec/wrongInput"
                        "test/FileSystem/FileOperations/copyDirRecursiveSpec/outputDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "copyDirRecursive, wrong input (symlink to directory)" $
      copyDirRecursive' "test/FileSystem/FileOperations/copyDirRecursiveSpec/wrongInputSymL"
                        "test/FileSystem/FileOperations/copyDirRecursiveSpec/outputDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    -- custom failures
    it "copyDirRecursive, destination in source" $
      copyDirRecursive' "test/FileSystem/FileOperations/copyDirRecursiveSpec/inputDir"
                        "test/FileSystem/FileOperations/copyDirRecursiveSpec/inputDir/foo"
        `shouldThrow`
        isDestinationInSource

