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
module MEnv.Screen.IOS
  (
    screenFBO,
    screenSize,
    screenShape,
    screenSetRate,

  ) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.Marshal.Alloc

import MyPrelude
import MEnv

import OpenGL


--------------------------------------------------------------------------------
--  FBO

foreign import ccall unsafe "ios_screenFBO" ios_screenFBO
    :: IO CUInt

-- | screen framebuffer
screenFBO :: MEnv res GLuint
screenFBO = io $ do
    fmap fI ios_screenFBO


--------------------------------------------------------------------------------
--  Size

foreign import ccall unsafe "ios_screenSize" ios_screenSize
    :: Ptr CUInt -> Ptr CUInt -> IO ()

-- | current screen size
screenSize :: MEnv res (UInt, UInt)
screenSize = io $ do
    alloca $ \ptrWth ->
      alloca $ \ptrHth -> do
          ios_screenSize ptrWth ptrHth
          wth <- peek ptrWth
          hth <- peek ptrHth
          return (fromIntegral wth, fromIntegral hth)


-- | current normalized screen size
screenShape :: MEnv res (Float, Float)
screenShape =
    screenSize >>= \(wth, hth) ->
        let scale  = 1 / (fromIntegral $ max wth hth)
        in return (scale * fromIntegral wth, scale * fromIntegral hth)



--------------------------------------------------------------------------------
--  Screen rate


foreign import ccall unsafe "ios_screenSetRate" ios_screenSetRate
    :: CUInt -> IO ()


-- | set screen update interval. 0 sets the value used when initializing screen.
screenSetRate :: UInt -> MEnv res ()
screenSetRate r = io $ 
    ios_screenSetRate (fI r)

     

