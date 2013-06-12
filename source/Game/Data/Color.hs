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
module Game.Data.Color
  (
    Color (..),

    uniformColor,
    smoothColor,

    colorNull,
    colorBlack,
    colorWhite,
    colorRed,
    colorGreen,
    colorBlue,
    colorYellow,

  ) where

import MyPrelude
import OpenGL
import OpenGL.Helpers


data Color =
    Color !GLfloat !GLfloat !GLfloat !GLfloat


-- | set vec4 uniform in GL from Color
uniformColor :: GLint -> Color -> IO ()
uniformColor uni (Color r g b a) = 
    glUniform4f uni r g b a


smoothColor :: Color -> Color -> Float -> Color
smoothColor (Color r0 g0 b0 a0) (Color r1 g1 b1 a1) alpha = 
    Color (smooth r0 r1 alpha) (smooth g0 g1 alpha) 
          (smooth b0 b1 alpha) (smooth a0 a1 alpha)
    where
      smooth x x' alpha =
          (1.0 - rTF alpha) * x + (rTF alpha) * x'



--------------------------------------------------------------------------------
--  color palette

colorNull :: Color
colorNull =
    Color 0.0 0.0 0.0 0.0

colorBlack :: Color
colorBlack =
    Color 0.0 0.0 0.0 1.0

colorWhite :: Color
colorWhite =
    Color 1.0 1.0 1.0 1.0

colorRed :: Color
colorRed = 
    Color 1.0 0.0 0.0 1.0

colorGreen :: Color
colorGreen = 
    Color 0.0 1.0 0.0 1.0

colorBlue :: Color
colorBlue = 
    Color 0.0 0.0 1.0 1.0

colorYellow:: Color
colorYellow = 
    Color 1.0 1.0 0.0 1.0

