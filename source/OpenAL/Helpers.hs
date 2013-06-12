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
module OpenAL.Helpers
  (
    genBuf,
    delBuf,
    loadBuf,

    genSrc,
    delSrc,

    getSrcState,

    listenerMat4,

    module Linear,
  ) where


import MyPrelude
import System.FilePath

import OpenAL
import Linear

--------------------------------------------------------------------------------
--  buffer

genBuf :: IO ALuint
genBuf = 
    alloca $ \ptr -> do
        alGenBuffers 1 ptr
        peek ptr

delBuf :: ALuint -> IO ()
delBuf src = 
    alloca $ \ptr -> do
        poke ptr src
        alDeleteBuffers 1 ptr

loadBuf :: ALuint -> FilePath -> IO ()
loadBuf buf path =
    withCString path $ \cstr -> 
        c_loadBuf buf cstr >>= \res -> case res of
            0   -> error $ "loadBuf: could not load " ++ takeFileName path
            _   -> return ()


#ifdef GRID_PLATFORM_IOS
foreign import ccall unsafe "ios_loadBuf" c_loadBuf 
    :: ALuint -> CString -> IO CUInt
#else
c_loadBuf = undefined
#endif


--------------------------------------------------------------------------------
--  source

genSrc :: IO ALuint
genSrc = 
    alloca $ \ptr -> do
        alGenSources 1 ptr
        peek ptr

delSrc :: ALuint -> IO ()
delSrc src = 
    alloca $ \ptr -> do
        poke ptr src
        alDeleteSources 1 ptr


getSrcState :: ALuint -> IO ALenum
getSrcState src = 
    alloca $ \ptr -> do
        alGetSourcei src al_SOURCE_STATE ptr
        fmap fI $ peek ptr


--------------------------------------------------------------------------------
--  

-- | set listener position and orientation from modelview matrix.
--   note: * velocity of listener is not set here
--         * not sure if this function is working properly
--   fixme: use projmodv instead?
listenerMat4 :: Mat4 -> IO ()
listenerMat4 (Mat4 a0 a1 a2 a3
                   b0 b1 b2 b3
                   c0 c1 c2 c3
                   d0 d1 d2 d3) = do
    -- pos
    alListener3f al_POSITION (-(rTF d0)) (-(rTF d1)) (-(rTF d2))
    
    -- dir, up
    allocaBytes 36 $ \ptr -> do
        pokeByteOff ptr 0  (-rTF a2 :: ALfloat) 
        pokeByteOff ptr 4  (-rTF b2 :: ALfloat) 
        pokeByteOff ptr 8  (-rTF c2 :: ALfloat) 
        pokeByteOff ptr 12 (rTF a1 :: ALfloat) 
        pokeByteOff ptr 16 (rTF b1 :: ALfloat) 
        pokeByteOff ptr 20 (rTF c1 :: ALfloat) 
        alListenerfv al_ORIENTATION ptr


