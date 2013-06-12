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
module GUI.Widget.LabelWidget
  (
    LabelWidget,

    makeLabelWidget,

    -- tmp:
    outputLabelWidget,

  ) where

import MyPrelude
import GUI.Widget
import GUI.Widget.Output



data LabelWidget a =
    LabelWidget
    {
        labelShape :: !GUIShape,
        labelString :: !String
    }


instance Widget LabelWidget where
    widgetShape = \gd gs w -> labelShape w
    widgetBegin = \gd gs w -> return w
    widgetEatInput = \gd gs w wi -> w
    widgetIterate = labelIterate



labelIterate :: GUIData -> GUIState -> LabelWidget a -> a -> IO (LabelWidget a, a)
labelIterate gd gs label a = do
    outputLabelWidget gd gs label
    return (label, a)


outputLabelWidget :: GUIData -> GUIState -> LabelWidget a -> IO ()
outputLabelWidget gd gs label = do
    useTexStencil gd gs 0 0 0
    draw4Shape gd gs (labelShape label)
    beginNoDepth gd gs
    drawTexts gd gs [(GUIPos 0.0 0.0, labelString label)]
    endNoDepth gd gs



--------------------------------------------------------------------------------
--  make


-- fixme: hth = 2.0 * unit (or guidataHth)

makeLabelWidget :: GUIData -> String -> LabelWidget a
makeLabelWidget gd str =
    let size = guidataFontSize gd
        charWth = fontdataCharWth (guidataFontData gd) size
        charHth = fontdataCharHth (guidataFontData gd) size
        wth = charWth * fI (length str)
        hth = charHth 
    in  LabelWidget
        {
            labelShape = GUIShape wth hth,
            labelString = str 
        }




