--
--   Copyright (c) 2013, Carl Joachim Svenn
--   All rights reserved.
--   
--   Redistribution and use in source and binary forms, with or without 
--   modification, are permitted provided that the following conditions are met:
--   
--       1. Redistributions of source code must retain the above copyright notice, this 
--          list of conditions and the following disclaimer.
--       2. Redistributions in binary form must reproduce the above copyright notice, 
--          this list of conditions and the following disclaimer in the documentation 
--          and/or other materials provided with the distribution.
--   
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
--   ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
--   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
--   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE 
--   FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
--   (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; 
--   LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND 
--   ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS 
--   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module MEnv.IOS
  (
    MEnv (..),
    runMEnvIOS,
    
    module MEnv.IOS.MEnvInit,

  ) where



import Foreign
import Foreign.Marshal.Alloc
import Data.IORef
import Control.Monad.Trans
import Control.Monad.State

import MEnv.IOS.MEnvInit


--------------------------------------------------------------------------------
--  MEnv

-- | the MEnv monad: computations inside an environment
newtype MEnv res a =
    MEnv
    {
        menvUnwrap :: StateT res IO a
    }
    deriving
      (
          Monad,
          MonadIO,
          MonadState res,
          Functor
      )



--------------------------------------------------------------------------------
--  runMEnvIOS


-- | init environment, then run MEnv inside. on iOS.
runMEnvIOS :: MEnvInit -> IO res -> (res -> IO ()) -> 
              (a -> MEnv res b) -> 
              (b -> MEnv res b) -> 
              (b -> MEnv res c) -> 
              a ->
              IO c
runMEnvIOS init loadResource unloadResource
           begin
           iterate
           end = \a -> do

    -- set init
    alloca $ \ptr -> do
        poke ptr init
        ios_init ptr
    
    refEnv <- newIORef $ error "runMEnvIOS: refEnv undefined"
    refB <- newIORef $ error "ruMEnvIOS: refB undefined"

    -- a -> m b
    -- create callback into Haskell from Foreign
    funptrBegin <- mkHaskellCall $ do
        -- create resource
        env <- loadResource

        (b, env') <- runStateT (menvUnwrap $ begin a) env
        writeIORef refB b
        writeIORef refEnv env'

    -- b -> m b
    -- create callback into Haskell from Foreign
    funptrIterate <- mkHaskellCall $ do
        env <- readIORef refEnv
        b <- readIORef refB
        (b', env') <- runStateT (menvUnwrap $ iterate b) env
        writeIORef refB b'
        writeIORef refEnv env'

    -- call Haskell from Foreign. this function should not return. 
    ios_main funptrBegin funptrIterate

    -- (the following code should not run in practice)

    freeHaskellFunPtr funptrIterate
    freeHaskellFunPtr funptrBegin

    -- b -> m c
    b <- readIORef refB
    env <- readIORef refEnv
    (c, env') <- runStateT (menvUnwrap $ end b) env

    unloadResource env'

    return c



-- | create 'void (*fun_ptr)()' of 'IO ()'
foreign import ccall "wrapper" mkHaskellCall
    :: IO () -> IO (FunPtr (IO ()))


-- | void ios_init(IOSInit* )
foreign import ccall "ios_init" ios_init
    :: Ptr MEnvInit -> IO ()

-- | void ios_main(void (*begin)(), void (*iterate)())
foreign import ccall safe "ios_main" ios_main
    :: FunPtr (IO ()) -> FunPtr (IO ()) -> IO ()







