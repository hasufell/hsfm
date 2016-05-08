{-# LANGUAGE OverloadedStrings #-}

module FileSystem.FileOperations.CopyFileOverwriteSpec where


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



spec :: Spec
spec =
  describe "HSFM.FileSystem.FileOperations.copyFileOverwrite" $ do

    -- successes --
    it "copyFileOverwrite, everything clear" $ do
      copyFileOverwrite' "test/FileSystem/FileOperations/copyFileOverwriteSpec/inputFile"
                "test/FileSystem/FileOperations/copyFileOverwriteSpec/outputFile"
      removeFileIfExists "test/FileSystem/FileOperations/copyFileOverwriteSpec/outputFile"

    it "copyFileOverwrite, output file already exists, all clear" $
      copyFileOverwrite' "test/FileSystem/FileOperations/copyFileOverwriteSpec/inputFile"
                "test/FileSystem/FileOperations/copyFileOverwriteSpec/alreadyExists"

    it "copyFileOverwrite, and compare" $ do
      copyFileOverwrite' "test/FileSystem/FileOperations/copyFileOverwriteSpec/inputFile"
                "test/FileSystem/FileOperations/copyFileOverwriteSpec/outputFile"
      (system $ "cmp -s " ++ "test/FileSystem/FileOperations/copyFileOverwriteSpec/inputFile" ++ " "
                          ++ "test/FileSystem/FileOperations/copyFileOverwriteSpec/outputFile")
        `shouldReturn` ExitSuccess
      removeFileIfExists "test/FileSystem/FileOperations/copyFileOverwriteSpec/outputFile"

    -- posix failures --
    it "copyFileOverwrite, input file does not exist" $
      copyFileOverwrite' "test/FileSystem/FileOperations/copyFileOverwriteSpec/noSuchFile"
                "test/FileSystem/FileOperations/copyFileOverwriteSpec/outputFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "copyFileOverwrite, no permission to write to output directory" $
      copyFileOverwrite' "test/FileSystem/FileOperations/copyFileOverwriteSpec/inputFile"
                "test/FileSystem/FileOperations/copyFileOverwriteSpec/outputDirNoWrite/outputFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyFileOverwrite, cannot open output directory" $
      copyFileOverwrite' "test/FileSystem/FileOperations/copyFileOverwriteSpec/inputFile"
                "test/FileSystem/FileOperations/copyFileOverwriteSpec/noPerms/outputFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyFileOverwrite, cannot open source directory" $
      copyFileOverwrite' "test/FileSystem/FileOperations/copyFileOverwriteSpec/noPerms/inputFile"
                "test/FileSystem/FileOperations/copyFileOverwriteSpec/outputFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyFileOverwrite, wrong input type (symlink)" $
      copyFileOverwrite' "test/FileSystem/FileOperations/copyFileOverwriteSpec/inputFileSymL"
                "test/FileSystem/FileOperations/copyFileOverwriteSpec/outputFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    it "copyFileOverwrite, wrong input type (directory)" $
      copyFileOverwrite' "test/FileSystem/FileOperations/copyFileOverwriteSpec/wrongInput"
                "test/FileSystem/FileOperations/copyFileOverwriteSpec/outputFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "copyFileOverwrite, output file already exists and is a dir" $
      copyFileOverwrite' "test/FileSystem/FileOperations/copyFileOverwriteSpec/inputFile"
                "test/FileSystem/FileOperations/copyFileOverwriteSpec/alreadyExistsD"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    -- custom failures --
    it "copyFileOverwrite, output and input are same file" $
      copyFileOverwrite' "test/FileSystem/FileOperations/copyFileOverwriteSpec/inputFile"
                "test/FileSystem/FileOperations/copyFileOverwriteSpec/inputFile"
        `shouldThrow` isSameFile
