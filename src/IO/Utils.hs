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
import Control.Monad
  (
    when
  , unless
  )


writeTVarIO :: TVar a -> a -> IO ()
writeTVarIO tvar val = atomically $ writeTVar tvar val


modifyTVarIO :: TVar a -> (a -> a) -> IO ()
modifyTVarIO tvar f = atomically $ modifyTVar tvar f


whenM :: Monad m => m Bool -> m () -> m ()
whenM mb a = mb >>= (`when` a)


unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb a = mb >>= (`unless` a)
