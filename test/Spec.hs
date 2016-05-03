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
import System.Posix.Files.ByteString
  (
    getSymbolicLinkStatus
  , groupExecuteMode
  , groupReadMode
  , nullFileMode
  , otherExecuteMode
  , otherReadMode
  , ownerExecuteMode
  , ownerReadMode
  , setFileMode
  , unionFileModes
  )
import Data.List
  (
    sort
  )




-- TODO: chardev, blockdev, namedpipe, socket


main :: IO ()
main = hspec $ before_ fixPermissions $ after_ revertPermissions $ do
  let tests = [copyFileSpec
              , copyDirRecursiveSpec
              , createDirSpec
              , createRegularFileSpec
              , renameFileSpec
              , moveFileSpec
              , recreateSymlinkSpec
              , deleteFileSpec
              , deleteDirSpec
              ]

  -- run all stateful tests twice to catch missing cleanups or state skew
  sequence_ tests
  sequence_ tests

  -- stateless tests
  getFileTypeSpec
  getDirsFilesSpec

  -- TODO: deleteFile, deleteDir, deleteDirRecursive, getDirsFiles, getFileType
  where
    noWriteDirs =  ["test/copyFileSpec/outputDirNoWrite"
                   ,"test/copyDirRecursiveSpec/noWritePerm"
                   ,"test/createDirSpec/noWritePerms"
                   ,"test/createRegularFileSpec/noWritePerms"
                   ,"test/renameFile/noWritePerm"
                   ,"test/moveFile/noWritePerm"
                   ,"test/recreateSymlinkSpec/noWritePerm"
                   ]
    noPermsDirs =  ["test/copyFileSpec/noPerms"
                   ,"test/copyDirRecursiveSpec/noPerms"
                   ,"test/createDirSpec/noPerms"
                   ,"test/createRegularFileSpec/noPerms"
                   ,"test/renameFile/noPerms"
                   ,"test/moveFile/noPerms"
                   ,"test/recreateSymlinkSpec/noPerms"
                   ,"test/getFileTypeSpec/noPerms"
                   ,"test/getDirsFilesSpec/noPerms"
                   , "test/deleteFileSpec/noPerms"
                   ]
    fixPermissions = do
      sequence_ $ fmap noWritableDirPerms noWriteDirs
      sequence_ $ fmap noPerms noPermsDirs
    revertPermissions = do
      sequence_ $ fmap normalDirPerms noWriteDirs
      sequence_ $ fmap normalDirPerms noPermsDirs


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

    it "copyFile, and compare" $
      copyFile'' "test/copyFileSpec/inputFile"
                "test/copyFileSpec/outputFile"
        (system $ "cmp -s " ++ "test/copyFileSpec/inputFile" ++ " "
                            ++ "test/copyFileSpec/outputFile")
        `shouldReturn` ExitSuccess

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


createDirSpec :: Spec
createDirSpec =
  describe "HSFM.FileSystem.FileOperations.createDir" $ do

    -- successes --
    it "createDir, all fine" $
      createDir' "test/createDirSpec/newDir"

    -- posix failures --
    it "createDir, can't write to output directory" $
      createDir' "test/createDirSpec/noWritePerms/newDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "createDir, can't open output directory" $
      createDir' "test/createDirSpec/noPerms/newDir"
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

    it "createRegularFile, can't write to destination directory" $
      createRegularFile' "test/createRegularFileSpec/noPerms/newDir"
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

    it "renameFile, can't write to output directory" $
      renameFile' "test/renameFile/myFile"
                  "test/renameFile/noWritePerm/renamedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "renameFile, can't open output directory" $
      renameFile' "test/renameFile/myFile"
                  "test/renameFile/noPerms/renamedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "renameFile, can't open source directory" $
      renameFile' "test/renameFile/noPerms/myFile"
                  "test/renameFile/renamedFile"
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

    it "moveFile, can't open destination directory" $
      moveFile' "test/moveFile/myFile"
                "test/moveFile/noPerms/movedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "moveFile, can't open source directory" $
      moveFile' "test/moveFile/noPerms/myFile"
                "test/moveFile/movedFile"
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

    it "recreateSymLink, can't open destination directory" $
      recreateSymlink' "test/recreateSymlinkSpec/myFileL"
                       "test/recreateSymlinkSpec/noPerms/movedFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)

    it "recreateSymLink, can't open source directory" $
      recreateSymlink' "test/recreateSymlinkSpec/noPerms/myFileL"
                       "test/recreateSymlinkSpec/movedFile"
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


getFileTypeSpec :: Spec
getFileTypeSpec =
  describe "HSFM.FileSystem.FileOperations.getFileType" $ do

    -- successes --
    it "getFileType, regular file" $
      getFileType' "test/getFileTypeSpec/regularfile"
        `shouldReturn` RegularFile

    it "getFileType, directory" $
      getFileType' "test/getFileTypeSpec/directory"
        `shouldReturn` Directory

    it "getFileType, directory with null permissions" $
      getFileType' "test/getFileTypeSpec/noPerms"
        `shouldReturn` Directory

    it "getFileType, symlink to file" $
      getFileType' "test/getFileTypeSpec/symlink"
        `shouldReturn` SymbolicLink

    it "getFileType, symlink to directory" $
      getFileType' "test/getFileTypeSpec/symlinkD"
        `shouldReturn` SymbolicLink

    it "getFileType, broken symlink" $
      getFileType' "test/getFileTypeSpec/brokenSymlink"
        `shouldReturn` SymbolicLink

    -- posix failures --
    it "getFileType, file does not exist" $
      getFileType' "test/getFileTypeSpec/nothingHere"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "getFileType, can't open directory" $
      getFileType' "test/getFileTypeSpec/noPerms/forz"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)


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


deleteFileSpec :: Spec
deleteFileSpec =
  describe "HSFM.FileSystem.FileOperations.deleteFile" $ do

    -- successes --
    it "deleteFile, regular file, all fine" $ do
      createRegularFile'' "test/deleteFileSpec/testFile"
      deleteFile' "test/deleteFileSpec/testFile"
      getSymbolicLinkStatus "test/deleteFileSpec/testFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "deleteFile, symlink, all fine" $ do
      recreateSymlink'' "test/deleteFileSpec/syml"
                        "test/deleteFileSpec/testFile"
      deleteFile' "test/deleteFileSpec/testFile"
      getSymbolicLinkStatus "test/deleteFileSpec/testFile"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    -- posix failures --
    it "deleteFile, wrong file type (directory)" $
      deleteFile' "test/deleteFileSpec/dir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "deleteFile, file does not exist" $
      deleteFile' "test/deleteFileSpec/doesNotExist"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "deleteFile, can't read directory" $
      deleteFile' "test/deleteFileSpec/noPerms/blah"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied)


deleteDirSpec :: Spec
deleteDirSpec =
  describe "HSFM.FileSystem.FileOperations.deleteDir" $ do

    -- successes --
    it "deleteDir, regular file, all fine" $ do
      createDir'' "test/deleteDirSpec/testDir"
      deleteDir' "test/deleteDirSpec/testDir"
      getSymbolicLinkStatus "test/deleteDirSpec/testDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "deleteDir, directory with null permissions, all fine" $ do
      createDir'' "test/deleteDirSpec/noPerms/testDir"
      noPerms "test/deleteDirSpec/noPerms/testDir"
      deleteDir' "test/deleteDirSpec/noPerms/testDir"

    -- posix failures --
    it "deleteDir, wrong file type (symlink to directory)" $
      deleteDir' "test/deleteDirSpec/dirSym"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "deleteDir, wrong file type (regular file)" $
      deleteDir' "test/deleteDirSpec/file"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "deleteDir, file does not exist" $
      deleteDir' "test/deleteDirSpec/doesNotExist"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "deleteDir, directory not empty" $
      deleteDir' "test/deleteDirSpec/dir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == UnsatisfiedConstraints)

    it "deleteDir, can't write to parent directory" $ do
      createDir'' "test/deleteDirSpec/noPerms/foo"
      noPerms "test/deleteDirSpec/noPerms"
      (deleteDir' "test/deleteDirSpec/noPerms/foo"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied))
        >> normalDirPerms "test/deleteDirSpec/noPerms"
        >> deleteDir' "test/deleteDirSpec/noPerms/foo"

deleteDirRecursiveSpec :: Spec
deleteDirRecursiveSpec =
  describe "HSFM.FileSystem.FileOperations.deleteDirRecursive" $ do

    -- successes --
    it "deleteDirRecursive, empty directory, all fine" $ do
      createDir'' "test/deleteDirRecursiveSpec/testDir"
      deleteDirRecursive' "test/deleteDirRecursiveSpec/testDir"
      getSymbolicLinkStatus "test/deleteDirRecursiveSpec/testDir"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    it "deleteDirRecursive, empty directory with null permissions, all fine" $ do
      createDir'' "test/deleteDirRecursiveSpec/noPerms/testDir"
      noPerms "test/deleteDirRecursiveSpec/noPerms/testDir"
      deleteDirRecursive' "test/deleteDirRecursiveSpec/noPerms/testDir"

    it "deleteDirRecursive, non-empty directory, all fine" $ do
      createDir'' "test/deleteDirRecursiveSpec/nonEmpty"
      createDir'' "test/deleteDirRecursiveSpec/nonEmpty/dir1"
      createDir'' "test/deleteDirRecursiveSpec/nonEmpty/dir2"
      createDir'' "test/deleteDirRecursiveSpec/nonEmpty/dir2/dir3"
      createRegularFile'' "test/deleteDirRecursiveSpec/nonEmpty/file1"
      createRegularFile'' "test/deleteDirRecursiveSpec/nonEmpty/dir1/file2"
      deleteDirRecursive' "test/deleteDirRecursiveSpec/nonEmpty"
      getSymbolicLinkStatus "test/deleteDirRecursiveSpec/nonEmpty"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)

    -- posix failures --
    it "deleteDirRecursive, can't open parent directory" $ do
      createDir'' "test/deleteDirRecursiveSpec/noPerms/foo"
      noPerms "test/deleteDirRecursiveSpec/noPerms"
      (deleteDirRecursive' "test/deleteDirRecursiveSpec/noPerms/foo"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied))
        >> normalDirPerms "test/deleteDirRecursiveSpec/noPerms"
        >> deleteDir' "test/deleteDirRecursiveSpec/noPerms/foo"

    it "deleteDirRecursive, can't write to parent directory" $ do
      createDir'' "test/deleteDirRecursiveSpec/noWritable/foo"
      noWritableDirPerms "test/deleteDirRecursiveSpec/noWritable"
      (deleteDirRecursive' "test/deleteDirRecursiveSpec/noWritable/foo"
        `shouldThrow`
        (\e -> ioeGetErrorType e == PermissionDenied))
      normalDirPerms "test/deleteDirRecursiveSpec/noWritable"
      deleteDir' "test/deleteDirRecursiveSpec/noWritable/foo"

    it "deleteDirRecursive, wrong file type (symlink to directory)" $
      deleteDirRecursive' "test/deleteDirRecursiveSpec/dirSym"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "deleteDirRecursive, wrong file type (regular file)" $
      deleteDirRecursive' "test/deleteDirRecursiveSpec/file"
        `shouldThrow`
        (\e -> ioeGetErrorType e == InappropriateType)

    it "deleteDirRecursive, directory does not exist" $
      deleteDirRecursive' "test/deleteDirRecursiveSpec/doesNotExist"
        `shouldThrow`
        (\e -> ioeGetErrorType e == NoSuchThing)





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


createDir'' :: ByteString -> IO ()
createDir'' dest = do
  pwd <- fromJust <$> getEnv "PWD" >>= P.parseAbs
  outputDir <- (pwd P.</>) <$> P.parseRel dest
  createDir outputDir


createRegularFile' :: ByteString -> IO ()
createRegularFile' dest = do
  pwd <- fromJust <$> getEnv "PWD" >>= P.parseAbs
  outputFile <- (pwd P.</>) <$> P.parseRel dest
  createRegularFile outputFile
  whenM (doesFileExist outputFile) (deleteFile outputFile)


createRegularFile'' :: ByteString -> IO ()
createRegularFile'' dest = do
  pwd <- fromJust <$> getEnv "PWD" >>= P.parseAbs
  outputFile <- (pwd P.</>) <$> P.parseRel dest
  createRegularFile outputFile


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


recreateSymlink'' :: ByteString -> ByteString -> IO ()
recreateSymlink'' inputFileP outputFileP = do
  pwd <- fromJust <$> getEnv "PWD" >>= P.parseAbs
  inputFile  <- (pwd P.</>) <$> P.parseRel inputFileP
  outputFile <- (pwd P.</>) <$> P.parseRel outputFileP
  recreateSymlink inputFile outputFile


noWritableDirPerms :: ByteString -> IO ()
noWritableDirPerms path = do
  pwd <- fromJust <$> getEnv "PWD" >>= P.parseAbs
  file <- (pwd P.</>) <$> P.parseRel path
  setFileMode (P.fromAbs file) perms
  where
    perms =            ownerReadMode
      `unionFileModes` ownerExecuteMode
      `unionFileModes` groupReadMode
      `unionFileModes` groupExecuteMode
      `unionFileModes` otherReadMode
      `unionFileModes` otherExecuteMode


noPerms :: ByteString -> IO ()
noPerms path = do
  pwd <- fromJust <$> getEnv "PWD" >>= P.parseAbs
  file <- (pwd P.</>) <$> P.parseRel path
  setFileMode (P.fromAbs file) nullFileMode


normalDirPerms :: ByteString -> IO ()
normalDirPerms path = do
  pwd <- fromJust <$> getEnv "PWD" >>= P.parseAbs
  file <- (pwd P.</>) <$> P.parseRel path
  setFileMode (P.fromAbs file) newDirPerms


getFileType' :: ByteString -> IO FileType
getFileType' path = do
  pwd <- fromJust <$> getEnv "PWD" >>= P.parseAbs
  file <- (pwd P.</>) <$> P.parseRel path
  getFileType file


getDirsFiles' :: ByteString -> IO [P.Path P.Abs]
getDirsFiles' path = do
  pwd <- fromJust <$> getEnv "PWD" >>= P.parseAbs
  file <- (pwd P.</>) <$> P.parseRel path
  getDirsFiles file


deleteFile' :: ByteString -> IO ()
deleteFile' p = do
  pwd <- fromJust <$> getEnv "PWD" >>= P.parseAbs
  file <- (pwd P.</>) <$> P.parseRel p
  deleteFile file


deleteDir' :: ByteString -> IO ()
deleteDir' p = do
  pwd <- fromJust <$> getEnv "PWD" >>= P.parseAbs
  file <- (pwd P.</>) <$> P.parseRel p
  deleteDir file


deleteDirRecursive' :: ByteString -> IO ()
deleteDirRecursive' p = do
  pwd <- fromJust <$> getEnv "PWD" >>= P.parseAbs
  dir <- (pwd P.</>) <$> P.parseRel p
  deleteDirRecursive dir
