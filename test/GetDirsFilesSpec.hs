{-# LANGUAGE OverloadedStrings #-}

module GetDirsFilesSpec where


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



getDirsFilesSpec :: Spec
getDirsFilesSpec =
  describe "HSFM.FileSystem.FileOperations.getDirsFiles" $ do

    -- successes --
    it "getDirsFiles, all fine" $ do
      pwd <- fromJust <$> getEnv "PWD" >>= P.parseAbs
      expectedFiles <- mapM P.parseRel ["test/getDirsFilesSpec/.hidden"
                                       ,"test/getDirsFilesSpec/Lala"
                                       ,"test/getDirsFilesSpec/dir"
                                       ,"test/getDirsFilesSpec/dirsym"
                                       ,"test/getDirsFilesSpec/file"
                                       ,"test/getDirsFilesSpec/noPerms"
                                       ,"test/getDirsFilesSpec/syml"]
      (fmap sort $ getDirsFiles' "test/getDirsFilesSpec")
        `shouldReturn` fmap (pwd P.</>) expectedFiles

    -- posix failures --
    it "getDirsFiles, nonexistent directory" $
      getDirsFiles' "test/getDirsFilesSpec/nothingHere"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "getDirsFiles, wrong file type (file)" $
      getDirsFiles' "test/getDirsFilesSpec/file"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "getDirsFiles, wrong file type (symlink to file)" $
      getDirsFiles' "test/getDirsFilesSpec/syml"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    it "getDirsFiles, wrong file type (symlink to dir)" $
      getDirsFiles' "test/getDirsFilesSpec/dirsym"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    it "getDirsFiles, can't open directory" $
      getDirsFiles' "test/getDirsFilesSpec/noPerms"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)




