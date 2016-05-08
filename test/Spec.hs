{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import FileSystem.FileOperations.CopyDirRecursiveSpec
import FileSystem.FileOperations.CopyDirRecursiveOverwriteSpec
import FileSystem.FileOperations.CopyFileOverwriteSpec
import FileSystem.FileOperations.CopyFileSpec
import FileSystem.FileOperations.CreateDirSpec
import FileSystem.FileOperations.CreateRegularFileSpec
import FileSystem.FileOperations.DeleteDirRecursiveSpec
import FileSystem.FileOperations.DeleteDirSpec
import FileSystem.FileOperations.DeleteFileSpec
import FileSystem.FileOperations.GetDirsFilesSpec
import FileSystem.FileOperations.GetFileTypeSpec
import FileSystem.FileOperations.MoveFileSpec
import FileSystem.FileOperations.RecreateSymlinkSpec
import FileSystem.FileOperations.RenameFileSpec
import Utils


-- TODO: chardev, blockdev, namedpipe, socket


main :: IO ()
main = hspec $ before_ fixPermissions $ after_ revertPermissions $ do
  let tests = [copyFileSpec
              ,copyFileOverwriteSpec
              ,copyDirRecursiveSpec
              ,copyDirRecursiveOverwriteSpec
              ,createDirSpec
              ,createRegularFileSpec
              ,renameFileSpec
              ,moveFileSpec
              ,recreateSymlinkSpec
              ,deleteFileSpec
              ,deleteDirSpec
              ,deleteDirRecursiveSpec
              ]

  -- run all stateful tests twice to catch missing cleanups or state skew
  sequence_ tests
  sequence_ tests

  -- stateless tests
  getFileTypeSpec
  getDirsFilesSpec

  where
    noWriteDirs =  ["test/FileSystem/FileOperations/copyFileSpec/outputDirNoWrite"
                   ,"test/FileSystem/FileOperations/copyFileOverwriteSpec/outputDirNoWrite"
                   ,"test/FileSystem/FileOperations/copyDirRecursiveSpec/noWritePerm"
                   ,"test/FileSystem/FileOperations/copyDirRecursiveOverwriteSpec/noWritePerm"
                   ,"test/FileSystem/FileOperations/createDirSpec/noWritePerms"
                   ,"test/FileSystem/FileOperations/createRegularFileSpec/noWritePerms"
                   ,"test/FileSystem/FileOperations/renameFileSpec/noWritePerm"
                   ,"test/FileSystem/FileOperations/moveFileSpec/noWritePerm"
                   ,"test/FileSystem/FileOperations/recreateSymlinkSpec/noWritePerm"
                   ]
    noPermsDirs =  ["test/FileSystem/FileOperations/copyFileSpec/noPerms"
                   ,"test/FileSystem/FileOperations/copyFileOverwriteSpec/noPerms"
                   ,"test/FileSystem/FileOperations/copyDirRecursiveSpec/noPerms"
                   ,"test/FileSystem/FileOperations/copyDirRecursiveOverwriteSpec/noPerms"
                   ,"test/FileSystem/FileOperations/createDirSpec/noPerms"
                   ,"test/FileSystem/FileOperations/createRegularFileSpec/noPerms"
                   ,"test/FileSystem/FileOperations/renameFileSpec/noPerms"
                   ,"test/FileSystem/FileOperations/moveFileSpec/noPerms"
                   ,"test/FileSystem/FileOperations/recreateSymlinkSpec/noPerms"
                   ,"test/FileSystem/FileOperations/getFileTypeSpec/noPerms"
                   ,"test/FileSystem/FileOperations/getDirsFilesSpec/noPerms"
                   ,"test/FileSystem/FileOperations/deleteFileSpec/noPerms"
                   ]
    fixPermissions = do
      sequence_ $ fmap noWritableDirPerms noWriteDirs
      sequence_ $ fmap noPerms noPermsDirs
    revertPermissions = do
      sequence_ $ fmap normalDirPerms noWriteDirs
      sequence_ $ fmap normalDirPerms noPermsDirs

