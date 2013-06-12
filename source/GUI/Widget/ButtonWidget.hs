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
module GUI.Widget.ButtonWidget
  (
    ButtonWidget,

    onPressA,
    onPressB,
    
    makeButtonWidget,

  ) where


import MyPrelude
import GUI.Widget
import GUI.Widget.Output
import GUI.Widget.Helpers
import GUI.Widget.ChildWidget



data ButtonWidget a =
    ButtonWidget
    {
        buttonChild :: !(ChildWidget a),

        buttonOK :: !Bool,
        buttonNotOKTick :: !GUITick,
        buttonIsPressA :: !Bool,
        buttonIsPressB :: !Bool,

        buttonActionPressA :: a -> a,
        buttonActionPressB :: a -> a

    }


--------------------------------------------------------------------------------
--  Widget structure


instance Widget ButtonWidget where
    widgetShape = buttonShape
    widgetBegin = buttonBegin
    widgetEatInput = buttonEatInput
    widgetIterate = buttonIterate


buttonShape :: GUIData -> GUIState -> ButtonWidget a -> GUIShape
buttonShape gd gs button = 
    widgetShape gd gs $ buttonChild button



buttonBegin :: GUIData -> GUIState -> ButtonWidget a -> 
               IO (ButtonWidget a)
buttonBegin gd gs button = 
    widgetBegin gd gs (buttonChild button) >>= \child' ->
        return $  button
                  {
                    buttonChild = child',
                    buttonIsPressA = False,
                    buttonIsPressB = False
                  }


buttonEatInput :: GUIData -> GUIState -> ButtonWidget a -> WidgetInput -> 
                  ButtonWidget a
buttonEatInput gd gs button wi =
    let child' = widgetEatInput gd gs (buttonChild button) wi
        button' = helper button wi
    in  button' { buttonChild = child' }
    
    where
      helper button wi =
          let pos = wiPos wi
              pos' = wiPos' wi
              ticks = wiTicks wi
          in case wiType wi of
                -- if touchin without movement for valuePressBTicks, then PressB
              WIDrag    -> let button' = 
                                  if ticks == 0.0 
                                     then button { buttonOK = True }
                                     else button
                               button'' = 
                                  if posIsAlmostEqual pos pos'
                                     then button'
                                     else button' 
                                          {
                                              buttonOK = False,
                                              buttonNotOKTick = guistateTick gs
                                          }

                           in if buttonOK button'' && valuePressBTicks <= ticks
                              then button''
                                   {
                                      buttonIsPressB = True,
                                      buttonOK = False,
                                      buttonNotOKTick = guistateTick gs
                                   }
                              else button''

                -- if released and no movement during touching and touching-time 
                -- strictly less than valuePressBTicks, then PressA
              WIDrop    -> if ticks < valuePressBTicks && buttonOK button
                           then button
                                { 
                                    buttonIsPressA = True,
                                    buttonOK = False,
                                    buttonNotOKTick = guistateTick gs
                                }
                           else button


buttonIterate :: GUIData -> GUIState -> ButtonWidget a -> a -> 
                 IO (ButtonWidget a, a)
buttonIterate gd gs button a = do

    -- modify if button press
    let a' | buttonIsPressA button = buttonActionPressA button a
           | buttonIsPressB button = buttonActionPressB button a
           | otherwise             = a

    -- iterate child
    (child', a'') <- widgetIterate gd gs (buttonChild button) a'
    
    return (button { buttonChild = child' }, a'')



--------------------------------------------------------------------------------
--  additional structure

onPressA :: ButtonWidget a -> (a -> a) -> ButtonWidget a
onPressA button action =
    button { buttonActionPressA = action }


onPressB :: ButtonWidget a -> (a -> a) -> ButtonWidget a
onPressB button action = 
    button { buttonActionPressB = action }


--------------------------------------------------------------------------------
--  make


makeButtonWidget :: Widget w => GUIData -> w a -> ButtonWidget a
makeButtonWidget gd w =
    ButtonWidget
    {
        buttonChild = makeChildWidget gd w,
        buttonActionPressA = id,
        buttonActionPressB = id,
        buttonIsPressA = False,
        buttonIsPressB = False,
        buttonOK = False,
        buttonNotOKTick = 0.0
    }


                  
--------------------------------------------------------------------------------
--  values

valueTweakOK :: Float 
valueTweakOK = 
    0.5

valueTweakTicksInv :: Float
valueTweakTicksInv =
    3.0
        
