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
module Main
  (
    main,

  ) where

import MyPrelude

#ifdef GRID_PLATFORM_IOS
import Main.IOS
#endif
#ifdef GRID_PLATFORM_GLFW
import Main.GLFW
#endif

#ifdef DEBUG
import OpenGL
import OpenGL.Helpers
import Foreign
#endif

main :: IO ()
main = do

#ifdef DEBUG
    -- we assume the following bitsizes in our code. 
    -- otherwise, the program will probably fail...
    assert (sizeOf (undefined :: GLubyte) == 1)   $ "sizeof GLubyte == 1"
    assert (sizeOf (undefined :: GLbyte) == 1)    $ "sizeof GLbyte == 1"
    assert (sizeOf (undefined :: GLushort) == 2)  $ "sizeof GLushort == 2"
    assert (sizeOf (undefined :: GLshort) == 2)   $ "sizeof GLshort == 2"
    assert (sizeOf (undefined :: GLfloat) == 4)   $ "sizeof GLfloat == 4"
#endif

    -- platform main
    main'









