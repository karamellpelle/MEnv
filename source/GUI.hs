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
module GUI
  (
    guiIterate,
    
    module GUI.Widget,
    module GUI.GUIData,
    module GUI.GUIShade,


  ) where


import MyPrelude
import GUI.GUIData
import GUI.GUIShade
import GUI.Widget
import Data.List

import OpenGL
import OpenGL.Helpers


guiIterate :: Widget w => 
    GUIShade -> GUIData -> Mat4 -> Float -> Float -> Float -> 
    [WidgetInput] -> GUITick -> w a -> a -> IO (w a, a)
guiIterate sh gd projmodv alpha wth hth inputs tick = \w a -> do

    glUniform2f (guiShadeUniPos sh) 0.0 0.0
    glUniform2f (guiShadeUniScale sh) 1.0 1.0
    glUniform1f (guiShadeUniDepth sh) 0.0
    glUniform1f (guiShadeUniAlpha sh) (rTF alpha)
    glUniform1f (guiShadeUniFillTexRepeat sh) (rTF $ guidataFillTexRepeat gd)
    glUniform1f (guiShadeUniFocus sh) 0.0 -- fixme: implement in shader
    glUniform1i (guiShadeUniUseTex sh) (fI $ gl_FALSE)
    glUniform1i (guiShadeUniUseFillTex sh) (fI $ gl_FALSE)
    glUniform1i (guiShadeUniUseStencil sh) (fI $ gl_FALSE)

    let gs =  GUIState
              {
                  guistateTick = tick,
                  
                  guistateGUIShade = sh,

                  guistateProjModv = projmodv,
                  guistateAlpha = alpha,
                  guistateWth = wth,
                  guistateHth = hth,
                  
                  guistatePos = GUIPos 0.0 0.0,
                  guistateScaleX = 1.0,
                  guistateScaleY = 1.0,
                  guistateDepth = 0.0,
                  guistateFocus = 0.0,
                  guistateFillTex = 0
              }

    -- begin widget
    w' <- widgetBegin gd gs w

    -- eat input
    let w'' = foldl' (\w wi -> widgetEatInput gd gs w wi) w' inputs 

    -- output widget + step widget
    widgetIterate gd gs w'' a



