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
module GUI.Widget.ScreenWidget
  (
    ScreenWidget,

    makeScreenWidget,

  ) where

import MyPrelude
import GUI.Widget
import GUI.Widget.ChildWidget
import GUI.Widget.Output
import GUI.Widget.Helpers



data ScreenWidget a =
    ScreenWidget
    {
        screenAlignX :: !Float,
        screenAlignY :: !Float,
        screenChild :: !(ChildWidget a)
    }


instance Widget ScreenWidget where
    widgetShape = screenShape
    widgetBegin = screenBegin
    widgetEatInput = screenEatInput
    widgetIterate = screenIterate



screenShape :: GUIData -> GUIState -> ScreenWidget a -> GUIShape
screenShape gd gs screen = 
    let wth = guistateWth gs
        hth = guistateHth gs
    in  GUIShape wth hth


screenBegin  :: GUIData -> GUIState -> ScreenWidget a -> IO (ScreenWidget a)
screenBegin gd gs screen = do
    child' <- widgetBegin gd gs $ screenChild screen
    return $ screen { screenChild = child' }


screenEatInput :: GUIData -> GUIState -> ScreenWidget a -> WidgetInput -> 
                  ScreenWidget a
screenEatInput gd gs screen wi = 
    let wth = guistateWth gs
        hth = guistateHth gs
        shape@(GUIShape wth' hth') = widgetShape gd gs $ screenChild screen
        x = screenAlignX screen * (wth - wth')
        y = screenAlignY screen * (hth - hth')
        child' = ifInputInsideThenElse gd gs (GUIPos x y) shape wi
                  (\gs' wi' -> widgetEatInput gd gs' (screenChild screen) wi')
                  (screenChild screen)

    in  screen { screenChild = child' }


screenIterate :: GUIData -> GUIState -> ScreenWidget a -> a -> IO (ScreenWidget a, a)
screenIterate gd gs screen a = do
    let wth = guistateWth gs
        hth = guistateHth gs
        GUIShape wth' hth' = widgetShape gd gs $ screenChild screen
        x = screenAlignX screen * (wth - wth')
        y = screenAlignY screen * (hth - hth')

    -- iterate child
    gs' <- plusPos gd gs $ GUIPos x y
    (child', a') <- widgetIterate gd gs' (screenChild screen) a
    -- fixme: reset pos??
    return (screen { screenChild = child' }, a')



--------------------------------------------------------------------------------
--  make


makeScreenWidget :: Widget w => GUIData -> Float -> Float -> w a -> ScreenWidget a
makeScreenWidget gd ax ay widget =
    ScreenWidget
    {
        screenAlignX = ax,
        screenAlignY = ay,
        screenChild = makeChildWidget gd widget
    }




