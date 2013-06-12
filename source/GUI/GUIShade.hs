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
module GUI.GUIShade
  (
    GUIShade (..),
    loadGUIShade,
    
    guiShade,

  ) where

import MyPrelude
import OpenGL
import OpenGL.Helpers
import OpenGL.Shade


data GUIShade =
    GUIShade
    {
        guiShadePrg :: !GLuint,
        guiShadeUniProjModvMatrix :: !GLint,
        guiShadeUniAlpha :: !GLint,

        guiShadeUniPos :: !GLint,
        guiShadeUniScale :: !GLint,
        guiShadeUniDepth :: !GLint,
        guiShadeUniFillTexRepeat :: !GLint,
        guiShadeUniFocus :: !GLint,
        
        guiShadeUniUseFillTex :: !GLint,
        guiShadeUniUseTex :: !GLint,
        guiShadeUniUseStencil :: !GLint,

        guiShadeUniStencilDim :: !GLint

    }


loadGUIShade :: FilePath -> IO GUIShade
loadGUIShade path = do
    prg <- createPrg (path ++ "/GUIShade.vsh") (path ++ "/GUIShade.fsh") [
                     (attPos, "a_pos"),
                     (attTexCoord, "a_tex_coord") ] [

                     (tex0, "u_tex"),
                     (tex1, "u_filltex"),
                     (tex2, "u_stencil") ]

    uProjModvMatrix <- getUniformLocation prg "u_projmodv_matrix"
    uAlpha <- getUniformLocation prg "u_alpha"
    uPos <- getUniformLocation prg "u_pos"
    uScale <- getUniformLocation prg "u_scale"
    uDepth <- getUniformLocation prg "u_depth"
    uFillTexRepeat <- getUniformLocation prg "u_filltex_repeat"
    uFocus <- getUniformLocation prg "u_focus"
    uUseFillTex <- getUniformLocation prg "u_use_filltex"
    uUseTex <- getUniformLocation prg "u_use_tex"
    uUseStencil <- getUniformLocation prg "u_use_stencil"
    uStencilDim <- getUniformLocation prg "u_stencil_dim"
    
    return  GUIShade
            {
                guiShadePrg = prg,
                guiShadeUniProjModvMatrix = uProjModvMatrix,
                guiShadeUniAlpha = uAlpha,

                guiShadeUniPos = uPos,
                guiShadeUniScale = uScale,
                guiShadeUniDepth = uDepth,
                guiShadeUniFillTexRepeat = uFillTexRepeat,
                guiShadeUniFocus = uFocus,
                
                guiShadeUniUseFillTex = uUseFillTex,
                guiShadeUniUseTex = uUseTex,
                guiShadeUniUseStencil = uUseStencil,

                guiShadeUniStencilDim = uStencilDim
            }
    


-- | 2D shading for GUI. y direction is top to bottom (read direction). hence
--   we use a left-handed coordinate system. 
--   this enables gl_DEPTH_TEST and disables gl_CULL_FACE
--   (FrontFace = CCW, CullFace disabled)
guiShade :: GUIShade -> Float -> Float -> Float -> IO Mat4
guiShade sh alpha wth hth = do

    glEnable gl_DEPTH_TEST
    glDisable gl_CULL_FACE

    glUseProgram $ guiShadePrg sh

    -- projection modelview
    let projmodv = mat4Ortho 0 wth hth 0 (-1) 1
    uniformMat4 (guiShadeUniProjModvMatrix sh) projmodv

    -- alpha
    glUniform1f (guiShadeUniAlpha sh) $ rTF alpha -- fixme: remove into guiIterate?

    return projmodv



