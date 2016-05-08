{--
HSFM, a filemanager written in Haskell.
Copyright (C) 2016 Julian Ospald

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
version 2 as published by the Free Software Foundation.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
--}

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK ignore-exports #-}


-- |This module provides high-level IO related file operations like
-- copy, delete, move and so on. It only operates on `Path Abs` which
-- guarantees us well-typed paths which are absolute.
--
-- Some functions are just path-safe wrappers around
-- unix functions, others have stricter exception handling
-- and some implement functionality that doesn't have a unix
-- counterpart (like `copyDirRecursive`).
--
-- Some of these operations are due to their nature not _atomic_, which
-- means they may do multiple syscalls which form one context. Some
-- of them also have to examine the filetypes explicitly before the
-- syscalls, so a reasonable decision can be made. That means
-- the result is undefined if another process changes that context
-- while the non-atomic operation is still happening. However, where
-- possible, as few syscalls as possible are used and the underlying
-- exception handling is kept.
module HSFM.FileSystem.UtilTypes where


import Data.ByteString
  (
    ByteString
  )
import HPath
  (
    Path
  , Abs
  , Fn
  )


-- |Data type describing file operations.
-- Useful to build up a list of operations or delay operations.
data FileOperation = FCopy    Copy
                   | FMove    Move
                   | FDelete  [Path Abs]
                   | FOpen    (Path Abs)
                   | FExecute (Path Abs) [ByteString]
                   | None


-- |Data type describing partial or complete file copy operation.
data Copy = PartialCopy [Path Abs] -- source files
          | Copy        [Path Abs] -- source files
                        (Path Abs) -- base destination directory


-- |Data type describing partial or complete file move operation.
data Move = PartialMove [Path Abs] -- source files
          | Move        [Path Abs] -- source files
                        (Path Abs) -- base destination directory


-- |Collision modes that describe the behavior in case a file collision
-- happens.
data FCollisonMode = Strict  -- ^ fail if the target already exists
                   | Overwrite
                   | OverwriteAll
                   | Skip
                   | Rename (Path Fn)

