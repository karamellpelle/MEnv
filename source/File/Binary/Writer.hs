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
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module File.Binary.Writer
  (
    Writer,

    writeBinary,
    writeBinary',

    wWord8,
    wWord8s,
    wNUL,
    wBool,
    wCString,
    wInt32,
    wUInt32,
    
    --wAlign,
    wCStringAlign, -- tmp!

    word32AsWord8s,


  ) where


import MyPrelude
import Text.Parsec
import System.IO
import Data.Binary.Put

-- Put only works with lazy bytestrings
--import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS

import Data.Char
import Data.Word
import Data.Bits

import Control.Exception as C

-- this is tmp!
#define PLATFORM_LE

{-
import GHC.Prim
import GHC.Exts
import GHC.Types
import GHC.Int
-}


--------------------------------------------------------------------------------
--  Writer


type Writer =
    Put



--------------------------------------------------------------------------------
--  write

writeBinary :: Writer -> FilePath -> IO (Maybe String)
writeBinary w path =
    C.catch (writeBinary' w path >> return Nothing) $ \e -> 
        return $ Just $ show (e :: IOException)



-- | assuming success, else error
writeBinary':: Writer -> FilePath -> IO ()
writeBinary' w path = do
    C.bracket (openBinaryFile path WriteMode) hClose $ \h -> do
        BS.hPut h $ runPut w




--------------------------------------------------------------------------------
--  

wWord8 :: Word8 -> Writer
wWord8 = 
    putWord8 


wWord8s :: [Word8] -> Writer
wWord8s =
    putLazyByteString . BS.pack -- putLazyByteString?


wNUL :: Writer
wNUL = 
    wWord8 0x00


wBool :: Bool -> Writer
wBool value = 
    case value of
        False   -> wWord8 0x01
        True    -> wWord8 0x02


-- | writing a C-string
wCString :: String -> Writer
wCString str = do
    wWord8s $ map (fI . ord) str
    wNUL

-- fixme: wAlign!
-- tmp solution: assuming n-alignment, padding with zeros
wCStringAlign :: UInt -> String -> Writer
wCStringAlign n str =
    case length' str + 1 of
        size -> do
            wCString str 
            wWord8s (replicate (fI $ (n - size `mod` n) `mod` n) 0x00)


-- | writing a value [-2^31, 2^31)
wInt32 :: Int -> Writer
wInt32 n =
    case word32AsWord8s (convert n) of
        (w0, w1, w2, w3)  -> wWord8s [w0, w1, w2, w3]

    where
{-
      convert (Int# n#) = 
          Word32# (unsafeCoerce# n#)
-}
      convert n = 
          if signum n == (-1) then complement (fI $ abs n) + 1 
                              else (fI $ abs n)


-- | writing a value [0, 2^32). 
wUInt32 :: UInt -> Writer
wUInt32 n = 
    case word32AsWord8s (fI n) of
       (w0, w1, w2, w3) -> wWord8s [w0, w1, w2, w3]


--------------------------------------------------------------------------------
-- 


-- | platform endian -> little endian 
word32AsWord8s :: Word32 -> (Word8, Word8, Word8, Word8)
word32AsWord8s w32 =
#ifdef PLATFORM_LE
    (fI $ 0x000000ff .&. (shiftR w32 0),
     fI $ 0x000000ff .&. (shiftR w32 8),
     fI $ 0x000000ff .&. (shiftR w32 16),
     fI $ 0x000000ff .&. (shiftR w32 24))
#else
     (fI $ 0x000000ff .&. (shiftR w32 24),
      fI $ 0x000000ff .&. (shiftR w32 16),
      fI $ 0x000000ff .&. (shiftR w32 8),
      fI $ 0x000000ff .&. (shiftR w32 0))
#endif 


