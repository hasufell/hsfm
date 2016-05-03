{-# LANGUAGE OverloadedStrings #-}

module FileSystem.FileOperations.CopyFileSpec where


import Test.Hspec
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



copyFileSpec :: Spec
copyFileSpec =
  describe "HSFM.FileSystem.FileOperations.copyFile" $ do

    -- successes --
    it "copyFile, everything clear" $ do
      copyFile' "test/FileSystem/FileOperations/copyFileSpec/inputFile"
                "test/FileSystem/FileOperations/copyFileSpec/outputFile"
      removeFileIfExists "test/FileSystem/FileOperations/copyFileSpec/outputFile"

    it "copyFile, and compare" $ do
      copyFile' "test/FileSystem/FileOperations/copyFileSpec/inputFile"
                "test/FileSystem/FileOperations/copyFileSpec/outputFile"
      (system $ "cmp -s " ++ "test/FileSystem/FileOperations/copyFileSpec/inputFile" ++ " "
                          ++ "test/FileSystem/FileOperations/copyFileSpec/outputFile")
        `shouldReturn` ExitSuccess
      removeFileIfExists "test/FileSystem/FileOperations/copyFileSpec/outputFile"

    -- posix failures --
    it "copyFile, input file does not exist" $
      copyFile' "test/FileSystem/FileOperations/copyFileSpec/noSuchFile"
                "test/FileSystem/FileOperations/copyFileSpec/outputFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "copyFile, no permission to write to output directory" $
      copyFile' "test/FileSystem/FileOperations/copyFileSpec/inputFile"
                "test/FileSystem/FileOperations/copyFileSpec/outputDirNoWrite/outputFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyFile, cannot open output directory" $
      copyFile' "test/FileSystem/FileOperations/copyFileSpec/inputFile"
                "test/FileSystem/FileOperations/copyFileSpec/noPerms/outputFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyFile, cannot open source directory" $
      copyFile' "test/FileSystem/FileOperations/copyFileSpec/noPerms/inputFile"
                "test/FileSystem/FileOperations/copyFileSpec/outputFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyFile, wrong input type (symlink)" $
      copyFile' "test/FileSystem/FileOperations/copyFileSpec/inputFileSymL"
                "test/FileSystem/FileOperations/copyFileSpec/outputFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    it "copyFile, wrong input type (directory)" $
      copyFile' "test/FileSystem/FileOperations/copyFileSpec/wrongInput"
                "test/FileSystem/FileOperations/copyFileSpec/outputFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "copyFile, output and input are same file" $
      copyFile' "test/FileSystem/FileOperations/copyFileSpec/inputFile"
                "test/FileSystem/FileOperations/copyFileSpec/inputFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    it "copyFile, output file already exists" $
      copyFile' "test/FileSystem/FileOperations/copyFileSpec/inputFile"
                "test/FileSystem/FileOperations/copyFileSpec/alreadyExists"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    it "copyFile, output file already exists and is a dir" $
      copyFile' "test/FileSystem/FileOperations/copyFileSpec/inputFile"
                "test/FileSystem/FileOperations/copyFileSpec/alreadyExistsD"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

