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
module GUI.Widget.BorderWidget
  (
    BorderWidget,

    makeBorderWidget,
    makeBorderWidgetSize,
  
  ) where



import MyPrelude
import GUI.Widget
import GUI.Widget.Output
import GUI.Widget.Helpers
import GUI.Widget.ChildWidget


data BorderWidget a =
    BorderWidget
    {
        borderChild :: !(ChildWidget a),
        borderSize :: !Float

    }



instance Widget BorderWidget where
    widgetShape = borderShape
    widgetBegin = borderBegin
    widgetEatInput = borderEatInput
    widgetIterate = borderIterate



--------------------------------------------------------------------------------
--  Widget structure


borderShape :: GUIData -> GUIState -> BorderWidget a -> GUIShape
borderShape gd gs = \border ->
    let size = borderSize border
        GUIShape wth hth = widgetShape gd gs (borderChild border)
    in  GUIShape (2.0 * size + wth) (2.0 * size + hth)


borderBegin :: GUIData -> GUIState -> BorderWidget a -> IO (BorderWidget a)
borderBegin gd gs border = do
    child' <- widgetBegin gd gs (borderChild border)
    return border { borderChild = child' }


borderEatInput :: GUIData -> GUIState -> BorderWidget a -> WidgetInput -> BorderWidget a
borderEatInput gd gs border wi =
    let gs' = gdgsPlusPos gd gs (GUIPos (borderSize border) (borderSize border))
    in  border { borderChild = widgetEatInput gd gs' (borderChild border) wi }


borderIterate :: GUIData -> GUIState -> BorderWidget a -> a -> IO (BorderWidget a, a)
borderIterate gd gs border a = do
   
    gs' <- plusPos gd gs (GUIPos (borderSize border) (borderSize border))

    -- draw FillTexBack before children
    gs'' <- useTexFillTexStencil gd gs' 0 (guidataFillTexBack gd) 
                                          (guidataBorderWidgetStencil gd) 0
    let shape = widgetShape gd gs'' (borderChild border)
    draw24ShapeAddSize gd gs'' shape (borderSize border)


    gs''' <- useFillTex gd gs'' (guidataFillTexMid gd)
      
    gs'''' <- incDepth gd gs'''

    -- iterate child
    (child', a') <- widgetIterate gd gs'''' (borderChild border) a

    resetDepth gd gs
    useTexStencil gd gs'''' 0 (guidataBorderWidgetStencil gd) 0
    draw24ShapeAddSize gd gs'''' shape (borderSize border)

    
    -- reset GUIState
    resetFillTex gd gs
    resetPos gd gs

    return (border { borderChild = child' }, a')
    
--------------------------------------------------------------------------------
--  make 


makeBorderWidgetSize :: Widget w => GUIData -> Float -> w a -> BorderWidget a
makeBorderWidgetSize gd size = \child -> 
    BorderWidget 
    {
        borderChild = makeChildWidget gd child,
        borderSize = size 
    }


makeBorderWidget :: Widget w => GUIData -> w a -> BorderWidget a
makeBorderWidget gd = \child -> 
    makeBorderWidgetSize gd size child
    where
      size = 0.02 -- ^ default size
