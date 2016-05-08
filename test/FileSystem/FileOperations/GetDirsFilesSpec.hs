{-# LANGUAGE OverloadedStrings #-}

module FileSystem.FileOperations.GetDirsFilesSpec where


import Data.List
  (
    sort
  )
import Data.Maybe
  (
    fromJust
  )
import qualified HPath as P
import Test.Hspec
import System.IO.Error
  (
    ioeGetErrorType
  )
import System.Posix.Env.ByteString
  (
    getEnv
  )
import GHC.IO.Exception
  (
    IOErrorType(..)
  )
import Utils



spec :: Spec
spec =
  describe "HSFM.FileSystem.FileOperations.getDirsFiles" $ do

    -- successes --
    it "getDirsFiles, all fine" $ do
      pwd <- fromJust <$> getEnv "PWD" >>= P.parseAbs
      expectedFiles <- mapM P.parseRel ["test/FileSystem/FileOperations/getDirsFilesSpec/.hidden"
                                       ,"test/FileSystem/FileOperations/getDirsFilesSpec/Lala"
                                       ,"test/FileSystem/FileOperations/getDirsFilesSpec/dir"
                                       ,"test/FileSystem/FileOperations/getDirsFilesSpec/dirsym"
                                       ,"test/FileSystem/FileOperations/getDirsFilesSpec/file"
                                       ,"test/FileSystem/FileOperations/getDirsFilesSpec/noPerms"
                                       ,"test/FileSystem/FileOperations/getDirsFilesSpec/syml"]
      (fmap sort $ getDirsFiles' "test/FileSystem/FileOperations/getDirsFilesSpec")
        `shouldReturn` fmap (pwd P.</>) expectedFiles

    -- posix failures --
    it "getDirsFiles, nonexistent directory" $
      getDirsFiles' "test/FileSystem/FileOperations/getDirsFilesSpec/nothingHere"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "getDirsFiles, wrong file type (file)" $
      getDirsFiles' "test/FileSystem/FileOperations/getDirsFilesSpec/file"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "getDirsFiles, wrong file type (symlink to file)" $
      getDirsFiles' "test/FileSystem/FileOperations/getDirsFilesSpec/syml"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    it "getDirsFiles, wrong file type (symlink to dir)" $
      getDirsFiles' "test/FileSystem/FileOperations/getDirsFilesSpec/dirsym"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    it "getDirsFiles, can't open directory" $
      getDirsFiles' "test/FileSystem/FileOperations/getDirsFilesSpec/noPerms"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)




