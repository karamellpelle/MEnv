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
module MEnv.IOS.MEnvInit
  (
    MEnvInit (..),

    ScreenOrientation (..),

  ) where

import MyPrelude
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.C.Types
import Data.Bits
import Data.List


data MEnvInit =
    MEnvInit
    {
        initScreenMultisample :: !UInt,                 -- ^ number of multisamples
        initScreenOrientations :: [ScreenOrientation],  -- ^ orientations
        initScreenRate :: !UInt,                        -- ^ skip frames
        initSoundSampleRate :: !UInt,                   -- ^ sound rate per second
        initKeysAcclGyroRate :: !Float                  -- ^ update interval in seconds.
                                                        --   0.0 disables
    }


data ScreenOrientation =
    OrientationPortrait         |
    OrientationPortraitFlipped  |
    OrientationLandscapeLeft    |
    OrientationLandscapeRight 



instance Storable MEnvInit where
    sizeOf _    = 4 + 4 + 4 + 4 + 4
    alignment _ = 4
    poke ptr init = do
        pokeByteOff ptr 0  (fI $ initScreenMultisample init :: CUInt)
        pokeByteOff ptr 4  (pokeOrientations $ initScreenOrientations init)
        pokeByteOff ptr 8  (fI$ initScreenRate init :: CUInt)
        pokeByteOff ptr 12 (fI $ initSoundSampleRate init :: CUInt)
        pokeByteOff ptr 16 (rTF $ initKeysAcclGyroRate init :: CFloat)
    peek ptr = do
        mult <- peekByteOff ptr 0 :: IO CUInt 
        orin <- peekByteOff ptr 4 :: IO CUInt
        rateScreen <- peekByteOff ptr 8 :: IO CUInt
        rateSound <- peekByteOff ptr 12 :: IO CUInt
        rateKeys <- peekByteOff ptr 16 :: IO CFloat

        return MEnvInit
               {
                  initScreenMultisample = fI mult,
                  initScreenOrientations = peekOrientations orin,
                  initScreenRate = fI rateScreen,
                  initSoundSampleRate = fI rateSound,
                  initKeysAcclGyroRate = rTF rateKeys
               }

pokeOrientations :: [ScreenOrientation] -> CUInt
pokeOrientations os = 
    foldl' step 0x00000000 os
    where
      step a o = case o of
          OrientationPortrait         -> a .|. 0x01
          OrientationPortraitFlipped  -> a .|. 0x02
          OrientationLandscapeLeft    -> a .|. 0x04
          OrientationLandscapeRight   -> a .|. 0x08

peekOrientations :: CUInt -> [ScreenOrientation]
peekOrientations a = 
    testA ++ testB ++ testC ++ testD
    where
      testA = if testBit a 0 then [OrientationPortrait]         else []
      testB = if testBit a 1 then [OrientationPortraitFlipped]  else []
      testC = if testBit a 2 then [OrientationLandscapeLeft]    else []
      testD = if testBit a 3 then [OrientationLandscapeRight]   else []

    

