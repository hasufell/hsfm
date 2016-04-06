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


-- |Random and general IO utilities.
module HSFM.Utils.IO where


import Control.Concurrent.STM
  (
    atomically
  )
import Control.Concurrent.STM.TVar
  (
    writeTVar
  , modifyTVar
  , TVar
  )
import Control.Monad
  (
    when
  , unless
  )


-- |Atomically write a TVar.
writeTVarIO :: TVar a -> a -> IO ()
writeTVarIO tvar val = atomically $ writeTVar tvar val


-- |Atomically modify a TVar.
modifyTVarIO :: TVar a -> (a -> a) -> IO ()
modifyTVarIO tvar f = atomically $ modifyTVar tvar f


-- |If the value of the first argument is True, then execute the action
-- provided in the second argument, otherwise do nothing.
whenM :: Monad m => m Bool -> m () -> m ()
whenM mb a = mb >>= (`when` a)


-- |If the value of the first argument is False, then execute the action
-- provided in the second argument, otherwise do nothing.
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb a = mb >>= (`unless` a)
