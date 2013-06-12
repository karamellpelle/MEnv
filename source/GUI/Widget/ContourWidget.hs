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
module GUI.Widget.ContourWidget
(
  ContourWidget,

  makeContourWidget,
  makeContourWidgetSize,

) where


import MyPrelude
import GUI.Widget
import GUI.Widget.Output
import GUI.Widget.ChildWidget

import OpenGL
import OpenGL.Helpers


data ContourWidget a =
  ContourWidget
  {
      contourChild :: !(ChildWidget a),
      contourSize :: !Float
  }


--  Widget structure
instance Widget ContourWidget where
  widgetShape = \gd gs w -> widgetShape gd gs (contourChild w)
  widgetBegin = contourBegin
  widgetEatInput = contourEatInput
  widgetIterate = contourIterate






contourBegin :: GUIData -> GUIState -> ContourWidget a -> IO (ContourWidget a)
contourBegin gd gs contour = 
    widgetBegin gd gs (contourChild contour) >>= \child' ->
        return contour { contourChild = child' }

contourEatInput :: GUIData -> GUIState -> ContourWidget a -> WidgetInput -> 
                   ContourWidget a
contourEatInput gd gs contour wi =
    let child' = widgetEatInput gd gs (contourChild contour) wi
    in  contour { contourChild = child' }


contourIterate :: GUIData -> GUIState -> ContourWidget a -> a -> 
                  IO (ContourWidget a, a)
contourIterate gd gs contour a = do
    -- iterate child
    (child', a') <- widgetIterate gd gs (contourChild contour) a


    -- draw contour 
    gs' <- incDepth gd gs
    useTexStencil gd gs' 0 (guidataContourWidgetStencil gd) 0
    glDepthMask gl_FALSE
    draw24ShapeSubSize gd gs (widgetShape gd gs contour) (contourSize contour) 
    glDepthMask gl_TRUE

    -- reset
    resetDepth gd gs


    return (contour { contourChild = child' }, a')


--------------------------------------------------------------------------------
--  make

makeContourWidgetSize :: Widget w => GUIData -> Float -> w a -> ContourWidget a
makeContourWidgetSize gd size = \w -> 
    ContourWidget
    {
        contourChild = makeChildWidget gd w,
        contourSize = size
    }


makeContourWidget :: Widget w => GUIData -> w a -> ContourWidget a
makeContourWidget gd = \w -> 
    makeContourWidgetSize gd size w
    where
        size = 0.08








