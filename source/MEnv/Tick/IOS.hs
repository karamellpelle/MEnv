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
module MEnv.Tick.IOS
  (
    Tick,
    tickGet,
    tickSet,
   
    tickClockAGet,
    tickClockASet,
    tickClockAGetSpeed,
    tickClockASetSpeed,

    tickClockBGet,
    tickClockBSet,
    tickClockBGetSpeed,
    tickClockBSetSpeed,

    tickClockCGet,
    tickClockCSet,
    tickClockCGetSpeed,
    tickClockCSetSpeed,

    tickClockDGet,
    tickClockDSet,
    tickClockDGetSpeed,
    tickClockDSetSpeed,
    
    tickClockEGet,
    tickClockESet,
    tickClockEGetSpeed,
    tickClockESetSpeed,

    tickClockFGet,
    tickClockFSet,
    tickClockFGetSpeed,
    tickClockFSetSpeed,
    
  ) where


import Foreign.C.Types
import MyPrelude
import MEnv



-- | time-type
type Tick =
    Double





--------------------------------------------------------------------------------
--  


foreign import ccall unsafe "ios_tickGet" ios_tickGet
    :: IO CDouble

-- | get current tick
tickGet :: MEnv res Tick
tickGet = io $ do
    fmap realToFrac ios_tickGet



foreign import ccall unsafe "ios_tickSet" ios_tickSet
    :: CDouble -> IO ()
    
-- | set current tick
tickSet :: Tick -> MEnv res ()
tickSet t = io $ do
    ios_tickSet (realToFrac t)
    


--------------------------------------------------------------------------------
-- clockA


foreign import ccall unsafe "ios_tickClockAGet" ios_tickClockAGet
    :: IO CDouble

-- | get tick for ClockA
tickClockAGet :: MEnv res Tick
tickClockAGet = io $ do
    fmap realToFrac ios_tickClockAGet


foreign import ccall unsafe "ios_tickClockASet" ios_tickClockASet
    :: CDouble -> IO ()

-- | set tick for ClockA
tickClockASet :: Tick -> MEnv res ()
tickClockASet t = io $ do
    ios_tickClockASet (realToFrac t)


foreign import ccall unsafe "ios_tickClockASetSpeed" ios_tickClockASetSpeed
    :: CDouble -> IO ()

-- | set speed of ClockA. 
--   todo: if speed is low, the clock runs less continuous. fix this.
tickClockASetSpeed :: Double -> MEnv res ()
tickClockASetSpeed a =  io $ do
    ios_tickClockASetSpeed (realToFrac a)


foreign import ccall unsafe "ios_tickClockAGetSpeed" ios_tickClockAGetSpeed
    :: IO CDouble

-- | get speed of ClockA
tickClockAGetSpeed :: MEnv res Double
tickClockAGetSpeed = io $ do
    fmap realToFrac ios_tickClockAGetSpeed


--------------------------------------------------------------------------------
-- clock B

foreign import ccall unsafe "ios_tickClockBGet" ios_tickClockBGet
    :: IO CDouble

-- | get tick for ClockB
tickClockBGet :: MEnv res Tick
tickClockBGet = io $ do
    fmap realToFrac ios_tickClockBGet


foreign import ccall unsafe "ios_tickClockBSet" ios_tickClockBSet
    :: CDouble -> IO ()

-- | set tick for ClockB
tickClockBSet :: Tick -> MEnv res ()
tickClockBSet t = io $ do
    ios_tickClockBSet (realToFrac t)


foreign import ccall unsafe "ios_tickClockBSetSpeed" ios_tickClockBSetSpeed
    :: CDouble -> IO ()

-- | set speed of ClockB. 
--   todo: if speed is low, the clock runs less continuous. fix this.
tickClockBSetSpeed :: Double -> MEnv res ()
tickClockBSetSpeed a =  io $ do
    ios_tickClockBSetSpeed (realToFrac a)


foreign import ccall unsafe "ios_tickClockBGetSpeed" ios_tickClockBGetSpeed
    :: IO CDouble

-- | get speed of ClockB
tickClockBGetSpeed :: MEnv res Double
tickClockBGetSpeed = io $ do
    fmap realToFrac ios_tickClockCGetSpeed


--------------------------------------------------------------------------------
-- clock C


foreign import ccall unsafe "ios_tickClockCGet" ios_tickClockCGet
    :: IO CDouble

-- | get tick for ClockC
tickClockCGet :: MEnv res Tick
tickClockCGet = io $ do
    fmap realToFrac ios_tickClockCGet


foreign import ccall unsafe "ios_tickClockCSet" ios_tickClockCSet
    :: CDouble -> IO ()

-- | set tick for ClockC
tickClockCSet :: Tick -> MEnv res ()
tickClockCSet t = io $ do
    ios_tickClockCSet (realToFrac t)


foreign import ccall unsafe "ios_tickClockCSetSpeed" ios_tickClockCSetSpeed
    :: CDouble -> IO ()

-- | set speed of ClockC. 
--   todo: if speed is low, the clock runs less continuous. fix this.
tickClockCSetSpeed :: Double -> MEnv res ()
tickClockCSetSpeed a =  io $ do
    ios_tickClockCSetSpeed (realToFrac a)


foreign import ccall unsafe "ios_tickClockCGetSpeed" ios_tickClockCGetSpeed
    :: IO CDouble

-- | get speed of ClockC
tickClockCGetSpeed :: MEnv res Double
tickClockCGetSpeed = io $ do
    fmap realToFrac ios_tickClockCGetSpeed


--------------------------------------------------------------------------------
-- clock D

foreign import ccall unsafe "ios_tickClockDGet" ios_tickClockDGet
    :: IO CDouble

-- | get tick for ClockD
tickClockDGet :: MEnv res Tick
tickClockDGet = io $ do
    fmap realToFrac ios_tickClockDGet


foreign import ccall unsafe "ios_tickClockDSet" ios_tickClockDSet
    :: CDouble -> IO ()

-- | set tick for ClockD
tickClockDSet :: Tick -> MEnv res ()
tickClockDSet t = io $ do
    ios_tickClockDSet (realToFrac t)


foreign import ccall unsafe "ios_tickClockDSetSpeed" ios_tickClockDSetSpeed
    :: CDouble -> IO ()

-- | set speed of ClockD. 
--   todo: if speed is low, the clock runs less continuous. fix this.
tickClockDSetSpeed :: Double -> MEnv res ()
tickClockDSetSpeed a =  io $ do
    ios_tickClockDSetSpeed (realToFrac a)


foreign import ccall unsafe "ios_tickClockDGetSpeed" ios_tickClockDGetSpeed
    :: IO CDouble

-- | get speed of ClockD
tickClockDGetSpeed :: MEnv res Double
tickClockDGetSpeed = io $ do
    fmap realToFrac ios_tickClockDGetSpeed
    
    
--------------------------------------------------------------------------------
-- clock E

foreign import ccall unsafe "ios_tickClockEGet" ios_tickClockEGet
    :: IO CDouble

-- | get tick for ClockE
tickClockEGet :: MEnv res Tick
tickClockEGet = io $ do
    fmap realToFrac ios_tickClockEGet


foreign import ccall unsafe "ios_tickClockESet" ios_tickClockESet
    :: CDouble -> IO ()

-- | set tick for ClockE
tickClockESet :: Tick -> MEnv res ()
tickClockESet t = io $ do
    ios_tickClockESet (realToFrac t)


foreign import ccall unsafe "ios_tickClockESetSpeed" ios_tickClockESetSpeed
    :: CDouble -> IO ()

-- | set speed of ClockE. 
--   todo: if speed is low, the clock runs less continuous. fix this.
tickClockESetSpeed :: Double -> MEnv res ()
tickClockESetSpeed a =  io $ do
    ios_tickClockESetSpeed (realToFrac a)


foreign import ccall unsafe "ios_tickClockEGetSpeed" ios_tickClockEGetSpeed
    :: IO CDouble

-- | get speed of ClockE
tickClockEGetSpeed :: MEnv res Double
tickClockEGetSpeed = io $ do
    fmap realToFrac ios_tickClockEGetSpeed


--------------------------------------------------------------------------------
-- clock F

foreign import ccall unsafe "ios_tickClockFGet" ios_tickClockFGet
    :: IO CDouble

-- | get tick for ClockF
tickClockFGet :: MEnv res Tick
tickClockFGet = io $ do
    fmap realToFrac ios_tickClockFGet


foreign import ccall unsafe "ios_tickClockFSet" ios_tickClockFSet
    :: CDouble -> IO ()

-- | set tick for ClockF
tickClockFSet :: Tick -> MEnv res ()
tickClockFSet t = io $ do
    ios_tickClockFSet (realToFrac t)


foreign import ccall unsafe "ios_tickClockFSetSpeed" ios_tickClockFSetSpeed
    :: CDouble -> IO ()

-- | set speed of ClockF. 
--   todo: if speed is low, the clock runs less continuous. fix this.
tickClockFSetSpeed :: Double -> MEnv res ()
tickClockFSetSpeed a =  io $ do
    ios_tickClockFSetSpeed (realToFrac a)


foreign import ccall unsafe "ios_tickClockFGetSpeed" ios_tickClockFGetSpeed
    :: IO CDouble

-- | get speed of ClockF
tickClockFGetSpeed :: MEnv res Double
tickClockFGetSpeed = io $ do
    fmap realToFrac ios_tickClockFGetSpeed
    








