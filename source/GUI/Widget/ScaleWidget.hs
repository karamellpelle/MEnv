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
module GUI.Widget.ScaleWidget
  (
    ScaleWidget,

    makeScaleWidget,

  ) where


import MyPrelude
import GUI.Widget
import GUI.Widget.Output
import GUI.Widget.ChildWidget





-- | ScaleWidget scales its child by specified amount.
data ScaleWidget a =
    ScaleWidget
    {
        scaleX :: Float,
        scaleY :: Float,
        scaleChild :: !(ChildWidget a)
    }


instance Widget ScaleWidget where
    widgetShape = scaleShape
    widgetBegin = scaleBegin
    widgetEatInput = scaleEatInput
    widgetIterate = scaleIterate



scaleShape :: GUIData -> GUIState -> ScaleWidget a -> GUIShape
scaleShape gd gs scale = 
    case widgetShape gd gs $ scaleChild scale of
        GUIShape wth hth -> GUIShape (wth * scaleX scale) (hth * scaleY scale)


scaleBegin :: GUIData -> GUIState -> ScaleWidget a -> IO (ScaleWidget a)
scaleBegin gd gs scale = do
    child' <- widgetBegin gd gs $ scaleChild scale
    return $ scale { scaleChild = child' }


scaleEatInput :: GUIData -> GUIState -> ScaleWidget a -> WidgetInput -> ScaleWidget a
scaleEatInput gd gs scale wi = 
    let gs' = gs { guistateScaleX = scaleX scale, guistateScaleY = scaleY scale }
        child' = widgetEatInput gd gs' (scaleChild scale) wi
    in  scale { scaleChild = child' }



scaleIterate :: GUIData -> GUIState -> ScaleWidget a -> a -> 
                IO (ScaleWidget a, a)
scaleIterate gd gs scale a = do
    gs' <- multScale gd gs (scaleX scale) (scaleY scale)
    (child', a') <- widgetIterate gd gs (scaleChild scale) a
    return (scale { scaleChild = child' }, a')



--------------------------------------------------------------------------------
--  make

makeScaleWidget :: Widget w => GUIData -> Float -> Float -> w a -> ScaleWidget a
makeScaleWidget gd ax ay w =
    ScaleWidget
    {
        scaleX = ax,
        scaleY = ay,
        scaleChild = makeChildWidget gd w
    }




