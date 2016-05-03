{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import CopyDirRecursiveSpec
import CopyFileSpec
import CreateDirSpec
import CreateRegularFileSpec
import DeleteDirRecursiveSpec
import DeleteDirSpec
import DeleteFileSpec
import GetDirsFilesSpec
import GetFileTypeSpec
import MoveFileSpec
import RecreateSymlinkSpec
import RenameFileSpec
import Utils



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
              , deleteDirRecursiveSpec
              ]

  -- run all stateful tests twice to catch missing cleanups or state skew
  sequence_ tests
  sequence_ tests

  -- stateless tests
  getFileTypeSpec
  getDirsFilesSpec

  where
    noWriteDirs =  ["test/copyFileSpec/outputDirNoWrite"
                   ,"test/copyDirRecursiveSpec/noWritePerm"
                   ,"test/createDirSpec/noWritePerms"
                   ,"test/createRegularFileSpec/noWritePerms"
                   ,"test/renameFileSpec/noWritePerm"
                   ,"test/moveFileSpec/noWritePerm"
                   ,"test/recreateSymlinkSpec/noWritePerm"
                   ]
    noPermsDirs =  ["test/copyFileSpec/noPerms"
                   ,"test/copyDirRecursiveSpec/noPerms"
                   ,"test/createDirSpec/noPerms"
                   ,"test/createRegularFileSpec/noPerms"
                   ,"test/renameFileSpec/noPerms"
                   ,"test/moveFileSpec/noPerms"
                   ,"test/recreateSymlinkSpec/noPerms"
                   ,"test/getFileTypeSpec/noPerms"
                   ,"test/getDirsFilesSpec/noPerms"
                   ,"test/deleteFileSpec/noPerms"
                   ]
    fixPermissions = do
      sequence_ $ fmap noWritableDirPerms noWriteDirs
      sequence_ $ fmap noPerms noPermsDirs
    revertPermissions = do
      sequence_ $ fmap normalDirPerms noWriteDirs
      sequence_ $ fmap normalDirPerms noPermsDirs

