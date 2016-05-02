{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import HSFM.FileSystem.FileOperations
import Data.Maybe
  (
    fromJust
  )
import qualified HPath as P
import System.Posix.Env.ByteString
  (
    getEnv
  )
import HSFM.FileSystem.Errors
import HSFM.Utils.IO
import System.IO.Error
  (
    ioeGetErrorType
  )
import GHC.IO.Exception
  (
    IOErrorType(..)
  )
import Data.ByteString
  (
    ByteString
  )
import System.Exit
import System.Process



-- TODO: chardev, blockdev, namedpipe, socket
-- TODO: no permission to open input file/dir


main :: IO ()
main = hspec $ do
  let tests = [copyFileSpec
              , copyDirRecursiveSpec
              , createDirSpec
              , createRegularFileSpec
              , renameFileSpec
              , moveFileSpec
              ,recreateSymlinkSpec
              ]

  -- run all tests twice to catch missing cleanups or state skew
  sequence_ tests
  sequence_ tests

  -- TODO: deleteFile, deleteDir, deleteDirRecursive, getDirsFiles, getFileType



    -------------
    --[ Specs ]--
    -------------


copyFileSpec :: Spec
copyFileSpec =
  describe "HSFM.FileSystem.FileOperations.copyFile" $ do

    -- successes --
    it "copyFile, everything clear" $
      copyFile' "test/copyFileSpec/inputFile"
                "test/copyFileSpec/outputFile"

    -- posix failures --
    it "copyFile, and compare" $
      copyFile'' "test/copyFileSpec/inputFile"
                "test/copyFileSpec/outputFile"
        (system $ "cmp -s " ++ "test/copyFileSpec/inputFile" ++ " "
                            ++ "test/copyFileSpec/outputFile")
        `shouldReturn` ExitSuccess

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


copyDirRecursiveSpec :: Spec
copyDirRecursiveSpec =
  describe "HSFM.FileSystem.FileOperations.copyDirRecursive" $ do

    -- successes --
    it "copyDirRecursive, all fine" $
      copyDirRecursive' "test/copyDirRecursiveSpec/inputDir"
                        "test/copyDirRecursiveSpec/outputDir"

    it "copyDirRecursive, all fine and compare" $
      copyDirRecursive'' "test/copyDirRecursiveSpec/inputDir"
                         "test/copyDirRecursiveSpec/outputDir"
        (system $ "diff -r --no-dereference "
                            ++ "test/copyDirRecursiveSpec/inputDir" ++ " "
                            ++ "test/copyDirRecursiveSpec/outputDir")
        `shouldReturn` ExitSuccess

    -- posix failures --
    it "copyDirRecursive, source directory does not exist" $
      copyDirRecursive' "test/copyDirRecursiveSpec/doesNotExist"
                        "test/copyDirRecursiveSpec/outputDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "copyDirRecursive, no write permission on destination dir" $
      copyDirRecursive' "test/copyDirRecursiveSpec/inputDir"
                        "test/copyDirRecursiveSpec/noWritePerm/foo"
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

    it "copyDirRecursive, destination and source same file" $
      copyDirRecursive' "test/copyDirRecursiveSpec/inputDir"
                        "test/copyDirRecursiveSpec/inputDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    it "copyDirRecursive, destination in source" $
      copyDirRecursive' "test/copyDirRecursiveSpec/inputDir"
                        "test/copyDirRecursiveSpec/inputDir/foo"
        `shouldThrow`
        isDestinationInSource

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


createDirSpec :: Spec
createDirSpec =
  describe "HSFM.FileSystem.FileOperations.createDir" $ do

    -- successes --
    it "createDir, all fine" $
      createDir' "test/createDirSpec/newDir"

    -- posix failures --
    it "createDir, can't write to destination directory" $
      createDir' "test/createDirSpec/noWritePerms/newDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "createDir, destination directory already exists" $
      createDir' "test/createDirSpec/alreadyExists"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)


createRegularFileSpec :: Spec
createRegularFileSpec =
  describe "HSFM.FileSystem.FileOperations.createRegularFile" $ do

    -- successes --
    it "createRegularFile, all fine" $
      createRegularFile' "test/createRegularFileSpec/newDir"

    -- posix failures --
    it "createRegularFile, can't write to destination directory" $
      createRegularFile' "test/createRegularFileSpec/noWritePerms/newDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "createRegularFile, destination file already exists" $
      createRegularFile' "test/createRegularFileSpec/alreadyExists"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)


renameFileSpec :: Spec
renameFileSpec =
  describe "HSFM.FileSystem.FileOperations.renameFile" $ do

    -- successes --
    it "renameFile, all fine" $
      renameFile' "test/renameFile/myFile"
                  "test/renameFile/renamedFile"

    it "renameFile, all fine" $
      renameFile' "test/renameFile/myFile"
                  "test/renameFile/dir/renamedFile"

    it "renameFile, all fine on symlink" $
      renameFile' "test/renameFile/myFileL"
                  "test/renameFile/renamedFile"

    it "renameFile, all fine on directory" $
      renameFile' "test/renameFile/dir"
                  "test/renameFile/renamedFile"

    -- posix failures --
    it "renameFile, source file does not exist" $
      renameFile' "test/renameFile/fileDoesNotExist"
                  "test/renameFile/renamedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "renameFile, can't write to destination directory" $
      renameFile' "test/renameFile/myFile"
                  "test/renameFile/noWritePerm/renamedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    -- custom failures --
    it "renameFile, destination file already exists" $
      renameFile' "test/renameFile/myFile"
                  "test/renameFile/alreadyExists"
        `shouldThrow`
        isFileDoesExist

    it "renameFile, move from file to dir" $
      renameFile' "test/renameFile/myFile"
                  "test/renameFile/alreadyExistsD"
        `shouldThrow`
        isDirDoesExist

    it "renameFile, source and dest are same file" $
      renameFile' "test/renameFile/myFile"
                  "test/renameFile/myFile"
        `shouldThrow`
        isSameFile


moveFileSpec :: Spec
moveFileSpec =
  describe "HSFM.FileSystem.FileOperations.moveFile" $ do

    -- successes --
    it "moveFile, all fine" $
      moveFile' "test/moveFile/myFile"
                "test/moveFile/movedFile"

    it "moveFile, all fine" $
      moveFile' "test/moveFile/myFile"
                "test/moveFile/dir/movedFile"

    it "moveFile, all fine on symlink" $
      moveFile' "test/moveFile/myFileL"
                "test/moveFile/movedFile"

    it "moveFile, all fine on directory" $
      moveFile' "test/moveFile/dir"
                "test/moveFile/movedFile"

    -- posix failures --
    it "moveFile, source file does not exist" $
      moveFile' "test/moveFile/fileDoesNotExist"
                "test/moveFile/movedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "moveFile, can't write to destination directory" $
      moveFile' "test/moveFile/myFile"
                "test/moveFile/noWritePerm/movedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    -- custom failures --
    it "moveFile, destination file already exists" $
      moveFile' "test/moveFile/myFile"
                "test/moveFile/alreadyExists"
        `shouldThrow`
        isFileDoesExist

    it "moveFile, move from file to dir" $
      moveFile' "test/moveFile/myFile"
                "test/moveFile/alreadyExistsD"
        `shouldThrow`
        isDirDoesExist

    it "moveFile, source and dest are same file" $
      moveFile' "test/moveFile/myFile"
                "test/moveFile/myFile"
        `shouldThrow`
        isSameFile


recreateSymlinkSpec :: Spec
recreateSymlinkSpec =
  describe "HSFM.FileSystem.FileOperations.recreateSymlink" $ do

    -- successes --
    it "recreateSymLink, all fine" $
      recreateSymlink' "test/recreateSymlinkSpec/myFileL"
                       "test/recreateSymlinkSpec/movedFile"

    it "recreateSymLink, all fine" $
      recreateSymlink' "test/recreateSymlinkSpec/myFileL"
                       "test/recreateSymlinkSpec/dir/movedFile"

    -- posix failures --
    it "recreateSymLink, wrong input type (file)" $
      recreateSymlink' "test/recreateSymlinkSpec/myFile"
                       "test/recreateSymlinkSpec/movedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    it "recreateSymLink, wrong input type (directory)" $
      recreateSymlink' "test/recreateSymlinkSpec/dir"
                       "test/recreateSymlinkSpec/movedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InvalidArgument)

    it "recreateSymLink, can't write to destination directory" $
      recreateSymlink' "test/recreateSymlinkSpec/myFileL"
                       "test/recreateSymlinkSpec/noWritePerm/movedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "recreateSymLink, destination file already exists" $
      recreateSymlink' "test/recreateSymlinkSpec/myFileL"
                       "test/recreateSymlinkSpec/alreadyExists"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    it "recreateSymLink, destination already exists and is a dir" $
      recreateSymlink' "test/recreateSymlinkSpec/myFileL"
                       "test/recreateSymlinkSpec/alreadyExistsD"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)

    it "recreateSymLink, source and destination are the same file" $
      recreateSymlink' "test/recreateSymlinkSpec/myFileL"
                       "test/recreateSymlinkSpec/myFileL"
        `shouldThrow`
        (\e -> ioeGetErrorType e == AlreadyExists)



    -----------------
    --[ Utilities ]--
    -----------------


copyFile'' :: ByteString -> ByteString -> IO a -> IO a
copyFile'' inputFileP outputFileP before_cleanup = do
  pwd <- fromJust <$> getEnv "PWD" >>= P.parseAbs
  inputFile  <- (pwd P.</>) <$> P.parseRel inputFileP
  outputFile <- (pwd P.</>) <$> P.parseRel outputFileP
  copyFile inputFile outputFile
  r <- before_cleanup
  whenM (doesFileExist outputFile) (deleteFile outputFile)
  return r


copyFile' :: ByteString -> ByteString -> IO ()
copyFile' inputFileP outputFileP =
  copyFile'' inputFileP outputFileP (return ())


copyDirRecursive'' :: ByteString -> ByteString -> IO a -> IO a
copyDirRecursive'' inputDirP outputDirP before_cleanup = do
  pwd <- fromJust <$> getEnv "PWD" >>= P.parseAbs
  inputDir  <- (pwd P.</>) <$> P.parseRel inputDirP
  outputDir <- (pwd P.</>) <$> P.parseRel outputDirP
  copyDirRecursive inputDir outputDir
  r <- before_cleanup
  whenM (doesDirectoryExist outputDir) (deleteDirRecursive outputDir)
  return r


copyDirRecursive' :: ByteString -> ByteString -> IO ()
copyDirRecursive' inputDirP outputDirP =
  copyDirRecursive'' inputDirP outputDirP (return ())


createDir' :: ByteString -> IO ()
createDir' dest = do
  pwd <- fromJust <$> getEnv "PWD" >>= P.parseAbs
  outputDir <- (pwd P.</>) <$> P.parseRel dest
  createDir outputDir
  whenM (doesDirectoryExist outputDir) (deleteDir outputDir)


createRegularFile' :: ByteString -> IO ()
createRegularFile' dest = do
  pwd <- fromJust <$> getEnv "PWD" >>= P.parseAbs
  outputFile <- (pwd P.</>) <$> P.parseRel dest
  createRegularFile outputFile
  whenM (doesFileExist outputFile) (deleteFile outputFile)


renameFile' :: ByteString -> ByteString -> IO ()
renameFile' inputFileP outputFileP = do
  pwd <- fromJust <$> getEnv "PWD" >>= P.parseAbs
  inputFile  <- (pwd P.</>) <$> P.parseRel inputFileP
  outputFile <- (pwd P.</>) <$> P.parseRel outputFileP
  renameFile inputFile outputFile
  renameFile outputFile inputFile


moveFile' :: ByteString -> ByteString -> IO ()
moveFile' inputFileP outputFileP = do
  pwd <- fromJust <$> getEnv "PWD" >>= P.parseAbs
  inputFile  <- (pwd P.</>) <$> P.parseRel inputFileP
  outputFile <- (pwd P.</>) <$> P.parseRel outputFileP
  moveFile inputFile outputFile
  moveFile outputFile inputFile


recreateSymlink' :: ByteString -> ByteString -> IO ()
recreateSymlink' inputFileP outputFileP = do
  pwd <- fromJust <$> getEnv "PWD" >>= P.parseAbs
  inputFile  <- (pwd P.</>) <$> P.parseRel inputFileP
  outputFile <- (pwd P.</>) <$> P.parseRel outputFileP
  recreateSymlink inputFile outputFile
  whenM (doesFileExist outputFile) (deleteFile outputFile)
