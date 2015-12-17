{-# OPTIONS_HADDOCK ignore-exports #-}

module IO.Utils where


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


writeTVarIO :: TVar a -> a -> IO ()
writeTVarIO tvar val = atomically $ writeTVar tvar val


modifyTVarIO :: TVar a -> (a -> a) -> IO ()
modifyTVarIO tvar f = atomically $ modifyTVar tvar f
