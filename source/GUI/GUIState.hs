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
module GUI.GUIState
  (
    GUIState (..),
    GUITick,
    GUIShape (..),
    GUIPos (..),

  ) where

import MyPrelude
import GUI.GUIShade

import OpenGL
import OpenGL.Helpers

-- (fixme: verify this)
-- INVARIANTS WHEN DRAWING A NEW WIDGET
-- 
-- * GUIState resembles GL-state, except FillTex
-- * all textures used shall be upside down (consistent with 2D drawing (FillTex))
-- * Program == ShadeGUI
-- * DepthTest enabled
-- * DepthFunc == GL_LEQUAL
-- * StencilTest disabled
-- * FrontFace == GL_CCW ? (vs 3D)
-- * CullFace disabled  ? (vs 3D)
-- * y-direction is top to bottom



data GUIState =
    GUIState
    {
        -- time
        guistateTick :: !GUITick,
        
        -- GL
        guistateGUIShade :: GUIShade,
        guistateAlpha :: !Float,
        guistateProjModv :: !Mat4,
        guistateWth :: !Float,
        guistateHth :: !Float,

        -- (fixme: more general, ProjModv :: Mat4)
        guistatePos :: !GUIPos,
        guistateScaleX :: !Float,
        guistateScaleY :: !Float,

        guistateDepth :: !Float,
        guistateFocus :: !Float,
        guistateFillTex :: !GLuint

    }


-- | GUIShape's are relative to vertex xy-space
data GUIShape =
    GUIShape
    {
        shapeWth :: !Float,
        shapeHth :: !Float
    }

-- | GUIPos's are relative to vertex xy-space
data GUIPos =
    GUIPos
    {
        posX :: !Float,
        posY :: !Float
    }


type GUITick =
    Double


