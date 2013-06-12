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
module MEnv.System.IOS
  (
    systemHandleFrontBegin,
    systemHandleFrontEnd,
    systemHandleBackBegin,
    systemHandleBackEnd,

  ) where

import MyPrelude
import MEnv
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array



-- | application: begin frontground
systemHandleFrontBegin :: a -> a -> MEnv res a
systemHandleFrontBegin a a' = io $ 
    ios_systemHandleFrontBegin >>= \value -> case value of
        0   -> return a
        _   -> return a'

foreign import ccall unsafe "ios_systemHandleFrontBegin" ios_systemHandleFrontBegin
    :: IO CUInt



-- | application: end frontground
systemHandleFrontEnd :: a -> a -> MEnv res a
systemHandleFrontEnd a a' = io $ 
    ios_systemHandleFrontEnd >>= \value -> case value of
        0   -> return a
        _   -> return a'

foreign import ccall unsafe "ios_systemHandleFrontEnd" ios_systemHandleFrontEnd
    :: IO CUInt



-- | application: begin background
systemHandleBackBegin :: a -> a -> MEnv res a
systemHandleBackBegin a a' = io $ 
    ios_systemHandleBackBegin >>= \value -> case value of
        0   -> return a
        _   -> return a'

foreign import ccall unsafe "ios_systemHandleBackBegin" ios_systemHandleBackBegin
    :: IO CUInt



-- | application: end background
systemHandleBackEnd :: a -> a -> MEnv res a
systemHandleBackEnd a a' = io $ 
    ios_systemHandleBackEnd >>= \value -> case value of
        0   -> return a
        _   -> return a'

foreign import ccall unsafe "ios_systemHandleBackEnd" ios_systemHandleBackEnd
    :: IO CUInt



