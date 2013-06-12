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
module GUI.Widget.MinWidget
  (
    MinWidget,

    makeMinWidget,

  ) where

import MyPrelude
import GUI.Widget
import GUI.Widget.ChildWidget
import GUI.Widget.Helpers
import GUI.Widget.Output

-- note: child draws outside minMinShape if greater shape


-- | MinWidget has shape min(minMinShape, childShape)
data MinWidget a =
    MinWidget
    {
        minChild :: !(ChildWidget a),
        minMinShape :: !GUIShape
    }




--------------------------------------------------------------------------------
--  Widget structure

instance Widget MinWidget where
    widgetShape = minShape
    widgetBegin = minBegin
    widgetEatInput = minEatInput
    widgetIterate = minIterate


minShape :: GUIData -> GUIState -> MinWidget a -> GUIShape
minShape gd gs min =
    let GUIShape wth hth = minMinShape min
        GUIShape wth' hth' = widgetShape gd gs $ minChild min

    in  GUIShape (Prelude.min wth wth') (Prelude.min hth hth')



minBegin :: GUIData -> GUIState -> MinWidget a -> IO (MinWidget a)
minBegin gd gs min = do
    child' <- widgetBegin gd gs (minChild min)
    return $ min { minChild = child' }


minEatInput :: GUIData -> GUIState -> MinWidget a -> WidgetInput -> MinWidget a
minEatInput gd gs min wi =
    let shape@(GUIShape wth hth) = widgetShape gd gs $ minChild min
        GUIShape wth' hth' = minMinShape min
        x = 0.5 * (Prelude.min wth wth' - wth)
        y = 0.5 * (Prelude.min hth hth' - hth)
        child' = ifInputInsideThenElse gd gs (GUIPos x y) shape wi
                 (\gs' wi' -> widgetEatInput gd gs' (minChild min) wi')
                 (minChild min)
    in  min { minChild = child' }


minIterate :: GUIData -> GUIState -> MinWidget a -> a -> IO (MinWidget a, a)
minIterate gd gs min a = do
    let GUIShape wth hth = widgetShape gd gs $ minChild min
        GUIShape wth' hth' = minMinShape min
        x = 0.5 * (Prelude.min wth wth' - wth)
        y = 0.5 * (Prelude.min hth hth' - hth)

    -- iterate child
    gs' <- plusPos gd gs $ GUIPos x y
    (child', a') <- widgetIterate gd gs' (minChild min) a
    
    return (min { minChild = child' }, a')




--------------------------------------------------------------------------------
--  make

makeMinWidget :: Widget w => GUIData -> GUIShape -> w a -> MinWidget a
makeMinWidget gd shape w =
    MinWidget
    {
        minChild = makeChildWidget gd w,
        minMinShape = shape
    }


