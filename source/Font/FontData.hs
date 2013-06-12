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
module Font.FontData
  (
    FontData (..),

    loadFontData,
    makeFontData,
    destroyFontData,

    fontdataCharSize,
    fontdataCharWth,
    fontdataCharHth,


  ) where

import MyPrelude
import OpenGL
import OpenGL.Helpers


data FontData =
    FontData
    {
      fontdataStencil :: !GLuint,
      fontdataCharAspect :: !Float,
      fontdataCharsX :: !UInt,
      fontdataCharsY :: !UInt,
      fontdataCharX :: !GLushort,
      fontdataCharY :: !GLushort,
      fontdataCharPadX :: !GLushort,
      fontdataCharPadY :: !GLushort,
      fontdataOffset :: !UInt
    }


--------------------------------------------------------------------------------
--  



-- | (implement some file format. parsec + zlib? load that.)
loadFontData :: FilePath -> IO FontData
loadFontData path = do
    error "fixme loadFontData"


-- | (so this function is tmp)
--   note: stencil image is assumed to be flipped vertically!
makeFontData :: FilePath -> UInt -> UInt -> Float -> Float -> UInt -> IO FontData
makeFontData path xs ys occupyX occupyY offset = do
    -- stencil
    stencil <- bindNewTex gl_TEXTURE_2D
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fI gl_CLAMP_TO_EDGE
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fI gl_CLAMP_TO_EDGE
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fI gl_LINEAR_MIPMAP_LINEAR
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fI gl_LINEAR
    (wth, hth) <- loadTexPreMult gl_TEXTURE_2D gl_RGBA $ path ++ "/tex.png" -- fixme: intfmt
    glGenerateMipmap gl_TEXTURE_2D

    -- stencil properties
    let aspect = (fI wth * occupyX * fI ys) / 
                 (fI hth * occupyY * fI xs)

        charx = (0xffff `div` fI xs)
        chary = (0xffff `div` fI ys) 
        padx = truncate $ (0.5 * 0xffff * (1.0 - occupyX)) / fI xs
        pady = truncate $ (0.5 * 0xffff * (1.0 - occupyY)) / fI ys
    
    return $  FontData
              {
                  fontdataStencil = stencil,
                  fontdataCharAspect = aspect,
                  fontdataCharsX = xs,
                  fontdataCharsY = ys,
                  fontdataCharX = charx,
                  fontdataCharY = chary,
                  fontdataCharPadX = padx,
                  fontdataCharPadY = pady,
                  fontdataOffset = offset
              }


destroyFontData :: FontData -> IO ()
destroyFontData fd = 
    delTex $ fontdataStencil fd


--------------------------------------------------------------------------------
--  

fontdataCharSize :: FontData -> Float -> (Float, Float)
fontdataCharSize fd size =
    (fontdataCharAspect fd * size, size)

fontdataCharWth :: FontData -> Float -> Float
fontdataCharWth fd size =
    fontdataCharAspect fd * size

fontdataCharHth :: FontData -> Float -> Float
fontdataCharHth fd size =
    size


