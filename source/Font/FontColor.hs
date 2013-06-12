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
module Font.FontColor
  (
    FontColor (..),
    makeFontColorFloat,
    makeFontColorGLubyte,

    fontColorScaleAlpha,
    fontColorChangeAlpha,

  ) where

import MyPrelude

import OpenGL
import OpenGL.Helpers


-- | fixme/question: 
--   * representing colors with premultiplied alpha 4-tuple?
--   * use 4 GLubyte's, instead of GLfloat? (but I don't think GHC saves
--     this space, and instead represent GLubyte as Word32) 
data FontColor =
    FontColor
    {
        fontcolorR :: !GLfloat,
        fontcolorG :: !GLfloat,
        fontcolorB :: !GLfloat,
        fontcolorA :: !GLfloat
    }


makeFontColorFloat :: Float -> Float -> Float -> Float -> FontColor
makeFontColorFloat r g b a =
    FontColor (rTF r) (rTF g) (rTF b) (rTF a)


makeFontColorGLubyte :: GLubyte -> GLubyte -> GLubyte -> GLubyte -> FontColor
makeFontColorGLubyte r g b a = 
    FontColor (0.003921569 * fI r) (0.003921569 * fI g) 
              (0.003921569 * fI b) (0.003921569 * fI a)


-- | fixme: premultiplied?
fontColorScaleAlpha :: FontColor -> Float -> FontColor
fontColorScaleAlpha (FontColor r g b a) scale = 
    FontColor r g b (rTF scale * a)


-- | fixme: premultiplied?
fontColorChangeAlpha :: FontColor -> Float -> FontColor
fontColorChangeAlpha (FontColor r g b a) a' =
    FontColor r g b (rTF a')

