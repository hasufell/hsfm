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


module HSFM.Settings where


import Data.ByteString
  (
    ByteString
  )
import Data.Maybe
import System.Posix.Env.ByteString
import System.Posix.Process.ByteString



    -----------------------
    --[ Common Settings ]--
    -----------------------




---- Command settings ----



-- |The terminal command. This should call `executeFile` in the end
-- with the appropriate arguments.
terminalCommand :: ByteString  -- ^ current directory of the FM
                -> IO a
terminalCommand cwd =
  executeFile   -- executes the given command
    "sakura"    -- the terminal command
    True        -- whether to search PATH
    ["-d", cwd] -- arguments for the command
    Nothing     -- optional custom environment: `Just [(String, String)]`


-- |The home directory. If you want to set it explicitly, you might
-- want to do:
--
-- @
-- home = return "\/home\/wurst"
-- @
home :: IO ByteString
home = fromMaybe <$> return "/" <*> getEnv "HOME"

