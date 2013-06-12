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
module MEnv.Foreign.IOS
  (
    foreignBeginForeign,
    foreignHandleForeignEnd,

  ) where

import MyPrelude
import MEnv
import Foreign.C



-- | start Foreign
foreignBeginForeign :: MEnv res ()
foreignBeginForeign = 
    io $ ios_foreignBeginForeign

foreign import ccall unsafe "ios_foreignBeginForeign" ios_foreignBeginForeign
    :: IO ()



-- | handle end of Foreign
foreignHandleForeignEnd :: a -> a -> MEnv res a
foreignHandleForeignEnd a a' =
    io $ ios_foreignHandleForeignEnd >>= \value -> case value of
        0   -> return a
        _   -> return a'

foreign import ccall unsafe "ios_foreignHandleForeignEnd" ios_foreignHandleForeignEnd
    :: IO CUInt

