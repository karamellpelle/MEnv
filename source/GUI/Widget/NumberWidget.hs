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
module GUI.Widget.NumberWidget
  (
    NumberWidget,

    makeNumberWidget,
    onNewNumber,

    -- tmp:
    outputNumberWidget,

  ) where

import MyPrelude
import GUI.Widget
import GUI.Widget.Output
import GUI.Widget.Helpers


data NumberWidget a =
    NumberWidget
    {
        numberShape :: !GUIShape,
        numberMin :: !Int,
        numberMax :: !Int,
        numberNumber :: !Int,
        numberIsDrag :: !Bool,
        numberDragTick :: !GUITick,
        numberIsNewNumber :: !Bool,
        numberActionNewNumber :: (Int -> a -> a)
    }


instance Widget NumberWidget where
    widgetShape = numberShape'
    widgetBegin = numberBegin
    widgetEatInput = numberEatInput
    widgetIterate = numberIterate


-- fixme: scale drag with guistateWth!

--------------------------------------------------------------------------------
--  


numberShape' :: GUIData -> GUIState -> NumberWidget a -> GUIShape
numberShape' gd gs number = 
    numberShape number


numberBegin :: GUIData -> GUIState -> NumberWidget a -> IO (NumberWidget a)
numberBegin gd gs number =
    return $ number { numberIsNewNumber = False }

    
numberEatInput :: GUIData -> GUIState -> NumberWidget a -> WidgetInput ->
                  NumberWidget a
numberEatInput gd gs number wi =
    case wiType wi of
        WIDrag    -> 
            dragNumber $ if wiIsTouched wi
                         then number 
                              {
                                  numberDragTick = wiTicks wi,
                                  numberIsDrag = True
                              }
                         else number
        WIDrop    -> 
            dropNumber number 
        -- _         -> number

    where
      dragNumber number =
          if numberIsDrag number 
              then let tick = numberDragTick number
                       tick' = wiTicks wi
                       xdrag = findXDrag wi
                   in if valueNumberRepeat <= (rTF tick' - rTF tick) * (xdrag * xdrag)
                      then let n' = keepInside (numberMin number) (numberMax number) $ 
                                    numberNumber number + floatSign xdrag
                           in  number
                               {
                                  numberNumber = n',
                                  numberDragTick = tick'
                               }
                      else number
              else number

      dropNumber number = 
          -- possible to change number with sweep 
          let n' = if numberDragTick number == 0.0
                   then keepInside (numberMin number) 
                                   (numberMax number) $ 
                                   numberNumber number + floatSign (findXDrag wi)
                   else numberNumber number
          in  number
              {
                numberNumber = n',
                numberIsDrag = False,
                numberIsNewNumber = True
              }

      findXDrag wi = 
          let GUIPos x _ = wiPos wi
              GUIPos x' _ = wiPos' wi
              xdrag = x' - x
          in  if valueXDragMin `absLEqual` xdrag
              then xdrag
              else 0.0

      absLEqual x y =
          x * x <= y * y

      floatSign x
          | x < 0.0 = -1
          | 0.0 < x = 1
          | otherwise = 0




numberIterate :: GUIData -> GUIState -> NumberWidget a -> a ->
                 IO (NumberWidget a, a)
numberIterate gd gs number a = do
    
    -- output
    outputNumberWidget gd gs number

    -- modify
    let a' = if numberIsNewNumber number 
             then let num = numberNumber number
                  in  numberActionNewNumber number num a
             else a

    return (number, a')


outputNumberWidget :: GUIData -> GUIState -> NumberWidget a -> IO ()
outputNumberWidget gd gs number = do
{- 
    -- draw background
    gs' <- useFillTexMid gd gs
    useNoStencil gd gs'
    draw4ShapeNoTex gd gs' $ numberShape number

-}
    -- draw number 
    case shapeScale 0.5 0.5 $ numberShape number of
        GUIShape wth hth  -> do
            drawCenteredTexts gd gs [(GUIPos wth hth, show (numberNumber number))]

    -- draw tex and fill
    gs' <- useTexFillTexStencil gd gs (guidataNumberWidgetTex gd) 0 0 0
    draw8ShapeSub gd gs' (numberShape number)
{-
    --beginNoDepth gd gs
    gs' <- useNoFillTex gd gs
    useTex gd gs' $ guidataNumberWidgetTex gd
    draw8ShapeSub gd gs' $ numberShape number
-}
    resetFillTex gd gs
    useTexStencil gd gs 0 (guidataNumberWidgetStencil gd) 0
    draw8 gd gs

{-
    setFillTex gd gs
    useNoTex gd gs
    useStencil gd gs $ guidataNumberWidgetStencil gd
    draw8 gd gs
-}
    --endNoDepth gd gs
    

--------------------------------------------------------------------------------
--  make


-- | make NumberWidget with numbers [min, max] (non-empty set)
makeNumberWidget :: GUIData -> Int -> Int -> Int -> NumberWidget a
makeNumberWidget gd nmin nmax n =
    let size = guidataFontSize gd
        (charWth, charHth) = fontdataCharSize (guidataFontData gd) size
        len = helper 10 1 $ if 0 <= nmin 
                            then nmax
                            else max nmax ((-nmin) + 1)
        shape = GUIShape (len * charWth + 2 * charHth) charHth

    in NumberWidget
       {
          numberShape = shape,
          numberMin = nmin,
          numberMax = nmax,
          numberNumber = n,
          numberIsDrag = False,
          numberDragTick = 0.0,
          numberIsNewNumber = False,
          numberActionNewNumber = \n a -> a
       }

    where
      -- find number of (digits + sign)
      helper a b x =
          if x < a then b
                   else helper (10 * a) (b + 1) x



--------------------------------------------------------------------------------
--  

-- | note: "new" does not mean n' != n
onNewNumber :: NumberWidget a -> (Int -> a -> a) -> NumberWidget a
onNewNumber number action =
    number { numberActionNewNumber = action }


--------------------------------------------------------------------------------
--  values

valueNumberRepeat :: Float
valueNumberRepeat = 
    0.005

valueXDragMin :: Float
valueXDragMin = 
    0.005
