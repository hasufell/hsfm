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

{-# OPTIONS_HADDOCK ignore-exports #-}

module HSFM.History where


import HPath
  (
    Abs
  , Path
  )



-- |Browsing history. For `forwardHistory` and `backwardsHistory`
-- the first item is the most recent one.
data BrowsingHistory = BrowsingHistory {
    backwardsHistory :: [Path Abs]
  , currentDir       :: Path Abs
  , forwardHistory   :: [Path Abs]
  , maxSize          :: Int
}


-- |This is meant to be called after e.g. a new path is entered
-- (not navigated to via the history) and the history needs updating.
goNewPath :: Path Abs -> BrowsingHistory -> BrowsingHistory
goNewPath p (BrowsingHistory b cd _ s) =
  BrowsingHistory (take s $ cd:b) p [] s


-- |Go back in the history.
goBack :: BrowsingHistory -> BrowsingHistory
goBack bh@(BrowsingHistory [] _ _ _)   = bh
goBack (BrowsingHistory (b:bs) cd fs s) =
  BrowsingHistory bs b (take s $ cd:fs) s


-- |Go forward in the history.
goForward :: BrowsingHistory -> BrowsingHistory
goForward bh@(BrowsingHistory _ _ [] _) = bh
goForward (BrowsingHistory bs cd (f:fs) s) =
  BrowsingHistory (take s $ cd:bs) f fs s

