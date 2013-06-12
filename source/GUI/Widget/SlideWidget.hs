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
module GUI.Widget.SlideWidget
  (
    SlideWidget,

    onNewValue,

    makeSlideWidget,

    -- tmp:
    outputSlideWidget,


  ) where

import MyPrelude
import MEnv
import GUI.Widget
import GUI.Widget.Helpers
import GUI.Widget.Output


data SlideWidget a =
    SlideWidget
    {
        slideInnerShape :: !GUIShape, -- fixme: outer shape!!
        --slideMin :: !Float,
        --slideMax :: !Float,

        slideIsDrag :: !Bool,
        slideX :: !Float,
        slideXIdeal :: !Float, 
        slideXTouched :: !Float,
        
        slideSubUnit :: !Float,         
        slideValuesWth :: !Float, 
        slideValuesWthInv :: !Float, 

        slideTick :: !GUITick,
        slideTickIdeal :: !GUITick,

        slideIsNewValue :: !Bool,
        slideActionNewValue :: Float -> a -> a
    }


instance Widget SlideWidget where
    widgetShape = slideShape
    widgetBegin = slideBegin
    widgetEatInput = slideEatInput
    widgetIterate = slideIterate



--------------------------------------------------------------------------------
--  


slideShape :: GUIData -> GUIState -> SlideWidget a -> GUIShape
slideShape gd gs slide =
    case slideInnerShape slide of
        GUIShape wth hth  -> GUIShape (wth + 2 * hth) hth


slideBegin :: GUIData -> GUIState -> SlideWidget a -> IO (SlideWidget a)
slideBegin gd gs slide =
    return $  slide
              {
                  slideIsNewValue = False,
                  slideTickIdeal = guistateTick gs
              }


slideEatInput :: GUIData -> GUIState -> SlideWidget a -> WidgetInput -> 
                 SlideWidget a
slideEatInput gd gs slide wi = 
    case wiType wi of
        WIDrag  ->  let GUIShape wth hth = slideInnerShape slide
                        GUIPos x y = guistatePos gs
                        wi' = wiRefPos wi (GUIPos (x + hth) y)
                    in  dragValue wi' $ 
                        if wiIsTouched wi'
                            then slide 
                                 {
                                    slideXTouched = findXTouched wi',
                                    slideIsDrag = True
                                 } 
                            else slide

        WIDrop  -> slide
                   { 
                      slideIsNewValue = True,
                      slideIsDrag = False
                   }


        --_       -> slide

        where
          dragValue wi = \slide -> 
              if slideIsDrag slide
                then let touched = slideXTouched slide
                         GUIPos x _ = wiPos wi
                         GUIPos x' _ = wiPos' wi
                         wth = slideValuesWth slide
                         ideal' = keepInside 0.0 wth $ 
                                  touched + valueIdealScale * (x' - x) 
                     in slide { slideXIdeal = ideal' }
                else slide 

          findXTouched wi =
            let wth = shapeWth $ slideInnerShape slide
                slidewth = slideSubUnit slide * wth
                GUIPos xrel _ = wiPos wi
                xideal = slideXIdeal slide
            in  if xrel < xideal || xideal + slidewth < xrel
                then xrel - 0.5 * slidewth
                else xideal


slideIterate :: GUIData -> GUIState -> SlideWidget a -> a -> 
                IO (SlideWidget a, a)
slideIterate gd gs slide a = do
    -- output
    outputSlideWidget gd gs slide

    -- step
    let tick = slideTick slide
        tick' = slideTickIdeal slide
        x' = stepDT (rTF tick' - rTF tick) 
                    (slideX slide) (slideXIdeal slide)
   
    -- modify
    let a' = if slideIsNewValue slide
             then let value = slideXIdeal slide * slideValuesWthInv slide
                  in  slideActionNewValue slide value a
             else a

    let slide' = slide
                 {
                    slideX = x',
                    slideTick = tick'
                 }

    return ( slide', a' )
    
    where
      stepDT dt z z' = 
          if z <= z' then min (z + dt * speed) z'
                     else max (z - dt * speed) z'
      speed = 128.0



outputSlideWidget :: GUIData -> GUIState -> SlideWidget a -> IO ()
outputSlideWidget gd gs slide = do
    -- translate
    let GUIShape wth hth = slideInnerShape slide
    gs' <- plusPos gd gs $ GUIPos hth 0.0

    -- draw contour
    useTexStencil gd gs' 0 (guidataSlideWidgetStencil gd) 0
    draw8ShapeAdd gd gs' $ slideInnerShape slide

    -- draw slider
    let x = slideX slide
        sliderwth = slideSubUnit slide * wth
    gs'' <- plusPos gd gs' $ GUIPos x 0.0
    gs''' <- useFillTex gd gs'' (guidataFillTexFront gd)
    useStencil gd gs''' (guidataSlideWidgetStencil gd) 1
    draw8ShapeAdd gd gs''' $ GUIShape sliderwth hth

    -- reset GUIState
    resetFillTex gd gs
    resetPos gd gs

--------------------------------------------------------------------------------
--  additional structure

onNewValue :: SlideWidget a -> (Float -> a -> a) -> SlideWidget a 
onNewValue slide action =
    slide { slideActionNewValue = action }




--------------------------------------------------------------------------------
--  make

-- | make SlideWidget with given width and subunit.
--   think of SubUnit as 1 page. if we want to scroll 5 pages with SlideWidget, 
--   then SubUnit is 1/5, i.e. 1 page is 1/5 of all pages.

makeSlideWidget :: GUIData -> Float -> Float -> Float -> SlideWidget a
makeSlideWidget gd wth subunit value =
    let innerWth = wth - 2.0 * innerHth
        innerHth = guidataFontSize gd
        valuesWth = (1.0 - subunit) * innerWth
        valuesWthInv = 1.0 / valuesWth
        x = value * valuesWth
    in SlideWidget
       {
          slideInnerShape = GUIShape innerWth innerHth,

          slideIsDrag = False,
          slideX = x,
          slideXIdeal = x,
          slideXTouched = 0.0,
          slideValuesWth = valuesWth,
          slideValuesWthInv = valuesWthInv,
          slideSubUnit = subunit,
          slideTick = 0.0,
          slideTickIdeal = 0.0,
          slideIsNewValue = False,
          slideActionNewValue = \v a -> a 
       }






--------------------------------------------------------------------------------
--  

valueIdealScale :: Float
valueIdealScale =
    1.0


