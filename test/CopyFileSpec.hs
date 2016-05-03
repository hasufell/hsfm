{-# LANGUAGE OverloadedStrings #-}

module CopyFileSpec where


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
      copyFile' "test/copyFileSpec/inputFile"
                "test/copyFileSpec/outputFile"
      removeFileIfExists "test/copyFileSpec/outputFile"

    it "copyFile, and compare" $ do
      copyFile' "test/copyFileSpec/inputFile"
                "test/copyFileSpec/outputFile"
      (system $ "cmp -s " ++ "test/copyFileSpec/inputFile" ++ " "
                          ++ "test/copyFileSpec/outputFile")
        `shouldReturn` ExitSuccess
      removeFileIfExists "test/copyFileSpec/outputFile"

    -- posix failures --
    it "copyFile, input file does not exist" $
      copyFile' "test/copyFileSpec/noSuchFile"
                "test/copyFileSpec/outputFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "copyFile, no permission to write to output directory" $
      copyFile' "test/copyFileSpec/inputFile"
                "test/copyFileSpec/outputDirNoWrite/outputFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyFile, cannot open output directory" $
      copyFile' "test/copyFileSpec/inputFile"
                "test/copyFileSpec/noPerms/outputFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyFile, cannot open source directory" $
      copyFile' "test/copyFileSpec/noPerms/inputFile"
                "test/copyFileSpec/outputFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "copyFile, wrong input type (symlink)" $
      copyFile' "test/copyFileSpec/inputFileSymL"
                "test/copyFileSpec/outputFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    it "copyFile, wrong input type (directory)" $
      copyFile' "test/copyFileSpec/wrongInput"
                "test/copyFileSpec/outputFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "copyFile, output and input are same file" $
      copyFile' "test/copyFileSpec/inputFile"
                "test/copyFileSpec/inputFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    it "copyFile, output file already exists" $
      copyFile' "test/copyFileSpec/inputFile"
                "test/copyFileSpec/alreadyExists"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    it "copyFile, output file already exists and is a dir" $
      copyFile' "test/copyFileSpec/inputFile"
                "test/copyFileSpec/alreadyExistsD"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

