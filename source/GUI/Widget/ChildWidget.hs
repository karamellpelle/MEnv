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
module GUI.Widget.ChildWidget
  (
    ChildWidget,

    makeChildWidget,

  ) where

import GUI.Widget



data ChildWidget a =
    ChildWidget
    {
        childShape' :: GUIData -> GUIState -> GUIShape,
        childBegin' :: GUIData -> GUIState -> IO (ChildWidget a),
        childEatInput' :: GUIData -> GUIState -> WidgetInput -> ChildWidget a,
        childIterate' :: GUIData -> GUIState -> a -> IO (ChildWidget a, a)
    }


instance Widget ChildWidget where
    widgetShape = \gd gs w -> childShape' w gd gs
    widgetBegin = \gd gs w -> childBegin' w gd gs
    widgetEatInput = \gd gs w wi -> childEatInput' w gd gs wi
    widgetIterate = \gd gs w a -> childIterate' w gd gs a



--------------------------------------------------------------------------------
--  make

makeChildWidget :: Widget w => GUIData -> w a -> ChildWidget a
makeChildWidget _ w =
    ChildWidget
    {
        childShape' = \gd gs -> widgetShape gd gs w,

        childBegin' = \gd gs -> widgetBegin gd gs w >>= \w' -> 
                                    return $ makeChildWidget gd w',

        childEatInput' = \gd gs wi -> let w' = widgetEatInput gd gs w wi
                                      in  makeChildWidget gd w',
                                    
        childIterate' = \gd gs a -> widgetIterate gd gs w a >>= \(w', a') ->
                                    return (makeChildWidget gd w', a')
    }




