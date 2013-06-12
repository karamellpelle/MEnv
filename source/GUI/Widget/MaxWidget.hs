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
module GUI.Widget.MaxWidget
  (
    MaxWidget,

    makeMaxWidget,

  ) where

import MyPrelude
import GUI.Widget
import GUI.Widget.ChildWidget
import GUI.Widget.Helpers
import GUI.Widget.Output




-- | MaxWidget has shape max(maxMaxShape, childShape)
data MaxWidget a =
    MaxWidget
    {
        maxChild :: !(ChildWidget a),
        maxMaxShape :: !GUIShape
    }




--------------------------------------------------------------------------------
--  Widget structure

instance Widget MaxWidget where
    widgetShape = maxShape
    widgetBegin = maxBegin
    widgetEatInput = maxEatInput
    widgetIterate = maxIterate


maxShape :: GUIData -> GUIState -> MaxWidget a -> GUIShape
maxShape gd gs max =
    let GUIShape wth hth = maxMaxShape max
        GUIShape wth' hth' = widgetShape gd gs $ maxChild max

    in  GUIShape (Prelude.max wth wth') (Prelude.max hth hth')


maxBegin :: GUIData -> GUIState -> MaxWidget a -> IO (MaxWidget a)
maxBegin gd gs max = do
    child' <- widgetBegin gd gs (maxChild max)
    return $ max { maxChild = child' }


maxEatInput :: GUIData -> GUIState -> MaxWidget a -> WidgetInput -> MaxWidget a
maxEatInput gd gs max wi =
    let shape@(GUIShape wth hth) = widgetShape gd gs $ maxChild max
        GUIShape wth' hth' = maxMaxShape max
        x = 0.5 * (Prelude.max wth wth' - wth)
        y = 0.5 * (Prelude.max hth hth' - hth)
        child' = ifInputInsideThenElse gd gs (GUIPos x y) shape wi
                 (\gs' wi' -> widgetEatInput gd gs' (maxChild max) wi')
                 (maxChild max)
    in  max { maxChild = child' }


maxIterate :: GUIData -> GUIState -> MaxWidget a -> a -> IO (MaxWidget a, a)
maxIterate gd gs max a = do
    let GUIShape wth hth = widgetShape gd gs $ maxChild max
        GUIShape wth' hth' = maxMaxShape max
        x = 0.5 * (Prelude.max wth wth' - wth)
        y = 0.5 * (Prelude.max hth hth' - hth)

    -- iterate child
    gs' <- plusPos gd gs $ GUIPos x y
    (child', a') <- widgetIterate gd gs' (maxChild max) a
    
    return (max { maxChild = child' }, a')




--------------------------------------------------------------------------------
--  make

makeMaxWidget :: Widget w => GUIData -> GUIShape -> w a -> MaxWidget a
makeMaxWidget gd shape w =
    MaxWidget
    {
        maxChild = makeChildWidget gd w,
        maxMaxShape = shape
    }


