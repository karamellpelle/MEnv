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
module File.Binary
  (

    module File.Binary.Reader,
    module File.Binary.Writer,

    module Text.Parsec,
    module Data.Word,
    
  ) where


import MyPrelude
import Text.Parsec
import Data.Word

import File.Binary.Reader
import File.Binary.Writer


--------------------------------------------------------------------------------
--  
-- todo:  * use 'binary' (and 'binary-strict'?) to write own parser library,
--          working similary as 'parsec'. this should fix 'fixme' below.
--        * implement zlib functionality, in order to work with files in compressed 
--          archives 
--
-- fixme: * platform endiannes (word8sToWord32, word32ToWord8s)
--        * remove line, columns, and instead talk about offset. then write the 
--          errormessages only with offset.
--        * remove additinal messages to 'fail "my message"'
--        * currently, failure messages are also wrong :(





{-

--------------------------------------------------------------------------------
--  debug

myRead :: Reader a -> [Word8] -> Either ParseError a
myRead ra ws =
    parse ra "myRead" (wrapByteString $ BS.pack ws)


myWrite :: Writer -> [Word8]
myWrite w = 
    BS.unpack $ BS.concat $ BSLazy.toChunks $ runPut w

testReadWrite :: Reader a -> (a -> Writer) -> a -> Either ParseError a
testReadWrite ra f a =
    myRead ra $ myWrite $ f a


minus1 :: [Word8]
minus1 = 
    [0x01, 0, 0, 0x80]

plus0 :: [Word8]
plus0 = [0, 0, 0, 0]


plus1 :: [Word8]
plus1 = [1, 0, 0, 0]

-}
