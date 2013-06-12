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
module Font.FontObject
  (
    FontObject (..),
    makeFontObject,
    destroyFontObject,
    writeFontObject,
    -- changeFontObjectColor, ...

  ) where

import MyPrelude
import Font.FontShade
import Font.FontData
import Font.FontColor
import Font.Buffer

import OpenGL
import OpenGL.Helpers
import OpenGL.Shade

-- note: since all fonts are drawn with one given ibo, the possible string length
--       is bounded

--------------------------------------------------------------------------------
--  


data FontObject =
    FontObject
    {
        fontobjectVAO :: !GLuint,
        fontobjectVBO :: !GLuint,
        fontobjectLen :: !UInt,
        -- changable properties
        fontobjectColor :: !FontColor,
        fontobjectCharSizeX :: !Float,
        fontobjectCharSizeY :: !Float,
        fontobjectStencil :: !GLuint
    }



--------------------------------------------------------------------------------
--  make / destroy


makeFontObject2D :: FontShade -> FontData -> Float -> FontColor -> String -> 
                    IO FontObject
makeFontObject2D =
    makeFontObject writePosStencilCoord2D


makeFontObject3D :: FontShade -> FontData -> Float -> FontColor -> String -> 
                    IO FontObject
makeFontObject3D =
    makeFontObject writePosStencilCoord3D



makeFontObject :: (FontData -> Ptr a -> String -> IO UInt) -> 
                  FontShade -> FontData -> Float -> FontColor -> String ->
                  IO FontObject
makeFontObject writePosStencilCoord sh fd size color str = do
    let len = fI $ length str

    -- vao 
    vao <- bindNewVAO
    glEnableVertexAttribArray attPos
    glEnableVertexAttribArray attStencilCoord

    -- vbo
    vbo <- bindNewBuf gl_ARRAY_BUFFER
    let bytesize = len * (4 * 8)
    glBufferData gl_ARRAY_BUFFER (fI bytesize) nullPtr gl_DYNAMIC_DRAW
    glVertexAttribPointer attPos 2 gl_UNSIGNED_SHORT gl_FALSE 8 $ mkPtrGLvoid 0
    glVertexAttribPointer attStencilCoord 2 gl_UNSIGNED_SHORT gl_FALSE 8 $ mkPtrGLvoid 4
    writeBuf gl_ARRAY_BUFFER $ \ptr -> 
        writePosLen ptr valueFontMaxCharacters

    return $  FontObject
              {
                  fontobjectVAO = vao,
                  fontobjectVBO = vbo,
                  fontobjectLen = len,
                  fontobjectColor = color,
                  fontobjectCharSizeX = size * fontdataCharAspect fd,
                  fontobjectCharSizeY = size,
                  fontobjectStencil = fontdataStencil fd

              }


destroyFontObject :: FontObject -> IO ()
destroyFontObject fo = do
    delBuf $ fontobjectVBO fo
    delBuf $ fontobjectVAO fo



--------------------------------------------------------------------------------
--  write


writeFontObject2D :: FontShade -> FontData -> FontObject -> String -> IO FontObject
writeFontObject2D =
    writeFontObject writeStencilCoord2D writePosStencilCoord2D


writeFontObject3D :: FontShade -> FontData -> FontObject -> String -> IO FontObject
writeFontObject3D =
    writeFontObject writeStencilCoord3D writePosStencilCoord3D


writeFontObject :: (FontData -> Ptr GLvoid -> String -> IO UInt) -> 
                   (FontData -> Ptr GLvoid -> String -> IO UInt) ->
                   FontShade -> FontData -> FontObject -> String -> IO FontObject
writeFontObject writeStencilCoord writePosStencilCoord sh fd fo str = do
    let len = fontobjectLen fo
        len' = fI $ length str

    -- create greater VBO, if necessary
    if len' <= len
      
      -- pos is written, write stencil coords
      then do
        glBindBuffer gl_ARRAY_BUFFER $ fontobjectVBO fo
        writeBuf gl_ARRAY_BUFFER $ \ptr -> 
            writeStencilCoord fd ptr str        

      -- create new buffer, write pos and stencil coords
      else do
        glBindBuffer gl_ARRAY_BUFFER $ fontobjectVBO fo
        let bytesize = len' * (4 * 8)
        glBufferData gl_ARRAY_BUFFER (fI bytesize) nullPtr gl_DYNAMIC_DRAW
        writeBuf gl_ARRAY_BUFFER $ \ptr -> 
            writePosStencilCoord fd ptr str     
    
    return $ fo { fontobjectLen = len' }


