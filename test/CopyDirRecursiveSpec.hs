{-# LANGUAGE OverloadedStrings #-}

module CopyDirRecursiveSpec where


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
      copyDirRecursive' "test/copyDirRecursiveSpec/inputDir"
                        "test/copyDirRecursiveSpec/outputDir"
      removeDirIfExists "test/copyDirRecursiveSpec/outputDir"

    it "copyDirRecursive, all fine and compare" $ do
      copyDirRecursive' "test/copyDirRecursiveSpec/inputDir"
                        "test/copyDirRecursiveSpec/outputDir"
      (system $ "diff -r --no-dereference "
                          ++ "test/copyDirRecursiveSpec/inputDir" ++ " "
                          ++ "test/copyDirRecursiveSpec/outputDir")
        `shouldReturn` ExitSuccess
      removeDirIfExists "test/copyDirRecursiveSpec/outputDir"

    -- posix failures --
    it "copyDirRecursive, source directory does not exist" $
      copyDirRecursive' "test/copyDirRecursiveSpec/doesNotExist"
                        "test/copyDirRecursiveSpec/outputDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "copyDirRecursive, no write permission on output dir" $
      copyDirRecursive' "test/copyDirRecursiveSpec/inputDir"
                        "test/copyDirRecursiveSpec/noWritePerm/foo"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyDirRecursive, cannot open output dir" $
      copyDirRecursive' "test/copyDirRecursiveSpec/inputDir"
                        "test/copyDirRecursiveSpec/noPerms/foo"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyDirRecursive, cannot open source dir" $
      copyDirRecursive' "test/copyDirRecursiveSpec/noPerms/inputDir"
                        "test/copyDirRecursiveSpec/foo"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyDirRecursive, destination dir already exists" $
      copyDirRecursive' "test/copyDirRecursiveSpec/inputDir"
                        "test/copyDirRecursiveSpec/alreadyExistsD"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    it "copyDirRecursive, destination already exists and is a file" $
      copyDirRecursive' "test/copyDirRecursiveSpec/inputDir"
                        "test/copyDirRecursiveSpec/alreadyExists"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    it "copyDirRecursive, destination and source same directory" $
      copyDirRecursive' "test/copyDirRecursiveSpec/inputDir"
                        "test/copyDirRecursiveSpec/inputDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    it "copyDirRecursive, wrong input (regular file)" $
      copyDirRecursive' "test/copyDirRecursiveSpec/wrongInput"
                        "test/copyDirRecursiveSpec/outputDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "copyDirRecursive, wrong input (symlink to directory)" $
      copyDirRecursive' "test/copyDirRecursiveSpec/wrongInputSymL"
                        "test/copyDirRecursiveSpec/outputDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    -- custom failures
    it "copyDirRecursive, destination in source" $
      copyDirRecursive' "test/copyDirRecursiveSpec/inputDir"
                        "test/copyDirRecursiveSpec/inputDir/foo"
        `shouldThrow`
        isDestinationInSource

