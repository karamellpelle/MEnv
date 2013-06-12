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
module OpenGL.Shade
  (
    tex0,
    tex1,
    tex2,
    tex3,
    attVec0,
    attVec1,
    attVec2,
    attVec3,
    attCoord0,
    attCoord1,
    attCoord2,
    attCoord3,

    attPos,
    attNormal,
    attColor,
    attTexCoord,
    attStencilCoord,

  ) where

import OpenGL



--------------------------------------------------------------------------------
--  textures

tex0 :: GLuint
tex0 = 0

tex1 :: GLuint
tex1 = 1

tex2 :: GLuint
tex2 = 2

tex3 :: GLuint
tex3 = 3




--------------------------------------------------------------------------------
--  vectors

attVec0 :: GLuint
attVec0 = 0

attVec1 :: GLuint
attVec1 = 1

attVec2 :: GLuint
attVec2 = 2

attVec3 :: GLuint
attVec3 = 3



--------------------------------------------------------------------------------
--  coordinates


attCoord0 :: GLuint
attCoord0 = 4

attCoord1 :: GLuint
attCoord1 = 5

attCoord2 :: GLuint
attCoord2 = 6

attCoord3 :: GLuint
attCoord3 = 7



--------------------------------------------------------------------------------
--  special

attPos :: GLuint
attPos = attVec0

attNormal :: GLuint
attNormal = attVec1

attColor :: GLuint
attColor = attVec2

attTexCoord :: GLuint
attTexCoord = attCoord0

attStencilCoord :: GLuint
attStencilCoord = attCoord1

