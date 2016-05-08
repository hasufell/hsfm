{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.Hspec.Runner
import Test.Hspec.Formatters
import qualified Spec
import Utils


-- TODO: chardev, blockdev, namedpipe, socket


main :: IO ()
main =
  hspecWith
    defaultConfig { configFormatter = Just progress }
    $ before_ fixPermissions
    $ after_ revertPermissions
    $ Spec.spec >> Spec.spec
  where
    noWriteDirs =  ["test/FileSystem/FileOperations/copyFileSpec/outputDirNoWrite"
                   ,"test/FileSystem/FileOperations/copyFileOverwriteSpec/outputDirNoWrite"
                   ,"test/FileSystem/FileOperations/copyDirRecursiveSpec/noWritePerm"
                   ,"test/FileSystem/FileOperations/copyDirRecursiveOverwriteSpec/noWritePerm"
                   ,"test/FileSystem/FileOperations/createDirSpec/noWritePerms"
                   ,"test/FileSystem/FileOperations/createRegularFileSpec/noWritePerms"
                   ,"test/FileSystem/FileOperations/renameFileSpec/noWritePerm"
                   ,"test/FileSystem/FileOperations/moveFileSpec/noWritePerm"
                   ,"test/FileSystem/FileOperations/moveFileOverwriteSpec/noWritePerm"
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
                   ,"test/FileSystem/FileOperations/moveFileOverwriteSpec/noPerms"
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


