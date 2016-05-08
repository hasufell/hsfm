{-# LANGUAGE OverloadedStrings #-}

module FileSystem.FileOperations.CopyDirRecursiveOverwriteSpec where


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



copyDirRecursiveOverwriteSpec :: Spec
copyDirRecursiveOverwriteSpec =
  describe "HSFM.FileSystem.FileOperations.copyDirRecursiveOverwrite" $ do

    -- successes --
    it "copyDirRecursiveOverwrite, all fine" $ do
      copyDirRecursiveOverwrite' "test/FileSystem/FileOperations/copyDirRecursiveOverwriteSpec/inputDir"
                                 "test/FileSystem/FileOperations/copyDirRecursiveOverwriteSpec/outputDir"
      removeDirIfExists "test/FileSystem/FileOperations/copyDirRecursiveOverwriteSpec/outputDir"

    it "copyDirRecursiveOverwrite, all fine and compare" $ do
      copyDirRecursiveOverwrite' "test/FileSystem/FileOperations/copyDirRecursiveOverwriteSpec/inputDir"
                                 "test/FileSystem/FileOperations/copyDirRecursiveOverwriteSpec/outputDir"
      (system $ "diff -r --no-dereference "
                          ++ "test/FileSystem/FileOperations/copyDirRecursiveOverwriteSpec/inputDir" ++ " "
                          ++ "test/FileSystem/FileOperations/copyDirRecursiveOverwriteSpec/outputDir")
        `shouldReturn` ExitSuccess
      removeDirIfExists "test/FileSystem/FileOperations/copyDirRecursiveOverwriteSpec/outputDir"

    it "copyDirRecursiveOverwrite, destination dir already exists" $
      copyDirRecursiveOverwrite' "test/FileSystem/FileOperations/copyDirRecursiveOverwriteSpec/inputDir"
                                 "test/FileSystem/FileOperations/copyDirRecursiveOverwriteSpec/alreadyExistsD"

    -- posix failures --
    it "copyDirRecursiveOverwrite, source directory does not exist" $
      copyDirRecursiveOverwrite' "test/FileSystem/FileOperations/copyDirRecursiveOverwriteSpec/doesNotExist"
                                 "test/FileSystem/FileOperations/copyDirRecursiveOverwriteSpec/outputDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "copyDirRecursiveOverwrite, no write permission on output dir" $
      copyDirRecursiveOverwrite' "test/FileSystem/FileOperations/copyDirRecursiveOverwriteSpec/inputDir"
                                 "test/FileSystem/FileOperations/copyDirRecursiveOverwriteSpec/noWritePerm/foo"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyDirRecursiveOverwrite, cannot open output dir" $
      copyDirRecursiveOverwrite' "test/FileSystem/FileOperations/copyDirRecursiveOverwriteSpec/inputDir"
                                 "test/FileSystem/FileOperations/copyDirRecursiveOverwriteSpec/noPerms/foo"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyDirRecursiveOverwrite, cannot open source dir" $
      copyDirRecursiveOverwrite' "test/FileSystem/FileOperations/copyDirRecursiveOverwriteSpec/noPerms/inputDir"
                                 "test/FileSystem/FileOperations/copyDirRecursiveOverwriteSpec/foo"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyDirRecursiveOverwrite, destination already exists and is a file" $
      copyDirRecursiveOverwrite' "test/FileSystem/FileOperations/copyDirRecursiveOverwriteSpec/inputDir"
                                 "test/FileSystem/FileOperations/copyDirRecursiveOverwriteSpec/alreadyExists"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "copyDirRecursiveOverwrite, wrong input (regular file)" $
      copyDirRecursiveOverwrite' "test/FileSystem/FileOperations/copyDirRecursiveOverwriteSpec/wrongInput"
                                 "test/FileSystem/FileOperations/copyDirRecursiveOverwriteSpec/outputDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "copyDirRecursiveOverwrite, wrong input (symlink to directory)" $
      copyDirRecursiveOverwrite' "test/FileSystem/FileOperations/copyDirRecursiveOverwriteSpec/wrongInputSymL"
                                 "test/FileSystem/FileOperations/copyDirRecursiveOverwriteSpec/outputDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    -- custom failures
    it "copyDirRecursiveOverwrite, destination in source" $
      copyDirRecursiveOverwrite' "test/FileSystem/FileOperations/copyDirRecursiveOverwriteSpec/inputDir"
                                 "test/FileSystem/FileOperations/copyDirRecursiveOverwriteSpec/inputDir/foo"
        `shouldThrow`
        isDestinationInSource

    it "copyDirRecursiveOverwrite, destination and source same directory" $
      copyDirRecursiveOverwrite' "test/FileSystem/FileOperations/copyDirRecursiveOverwriteSpec/inputDir"
                                 "test/FileSystem/FileOperations/copyDirRecursiveOverwriteSpec/inputDir"
        `shouldThrow`
        isSameFile
