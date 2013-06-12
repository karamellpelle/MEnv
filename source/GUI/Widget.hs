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
module GUI.Widget
  (
    Widget (..),

    WidgetInput (..),
    WidgetInputType (..),

    module GUI.GUIData,
    module GUI.GUIState,

  ) where


import OpenGL
import GUI.GUIData
import GUI.GUIState


-- | note: currently, GUIState is only updated when widgetEatInput, widgetIterate
class Widget w where
    widgetShape :: GUIData -> GUIState -> w a -> GUIShape
    widgetBegin :: GUIData -> GUIState -> w a -> IO (w a)
    widgetEatInput :: GUIData -> GUIState -> w a -> WidgetInput -> w a 
    widgetIterate :: GUIData -> GUIState -> w a -> a -> IO (w a, a)


-- | WidgetInput's are always relative to vertex xy-space
data WidgetInput =
    WidgetInput
    {
        -- general data (actually pretty special :) )
        wiPos :: !GUIPos,
        wiPos' :: !GUIPos,
        wiTicks :: !GUITick,
        
        -- special data
        wiType :: !WidgetInputType
    }

data WidgetInputType =
    WIDrag    |
    WIDrop
    
-- note: WidgetInputs are (currently) only delivered to a widget if pos or pos' of
--       input is inside the widget. this is important for container widgets
--       (typically using ChildWidget's)



