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
module GUI.Widget.Helpers
  (
    wiIsTouched,
    wiIsReleased,
    wiRefPos,
    ifInputInsideThenElse,
    gdgsIsInputInside,

    posRef,
    posPlus,
    posScalePlus,
    posScale,
    posIsInside,
    posIsAlmostEqual,

    shapeScale,

    gdgsPlusPos,
    gdgsIsPosInside,
    --posInsideShape,
    --posInsideShape',

    --originInput,
  
    --beginChildren,
    --eatinputModifyBeforeChild,
    --eatinputModifyBeforeChildren,
    --eatinputModifyBeforeEachChild,
    --iterateChildren,

  ) where

import GUI.Widget
import Control.Monad


--------------------------------------------------------------------------------
--  input

-- | touch begin? (assuming t == 0 iff touched)
wiIsTouched :: WidgetInput -> Bool
wiIsTouched wi =
    case wiType wi of
        WIDrag    -> wiTicks wi == 0.0
        _         -> False


-- | touch end?
wiIsReleased :: WidgetInput -> Bool
wiIsReleased wi =
    case wiType wi of
        WIDrop    -> True
        _         -> False


wiRefPos :: WidgetInput -> GUIPos -> WidgetInput
wiRefPos wi ref = 
    wi
    {
        wiPos = posRef ref (wiPos wi),
        wiPos' = posRef ref (wiPos' wi)
    }    


-- remove this...
ifInputInsideThenElse :: GUIData -> GUIState -> GUIPos -> GUIShape -> WidgetInput -> 
                         (GUIState -> WidgetInput -> b) -> (b) -> b
ifInputInsideThenElse gd gs pos shape wi fThen fElse = 
    let spos = guistatePos gs 
        ax = guistateScaleX gs
        ay = guistateScaleY gs
        spos' = posScalePlus spos ax ay pos
        shape' = shapeScale ax ay shape
    in  if posIsInside spos' shape' (wiPos wi) ||
           posIsInside spos' shape' (wiPos' wi)
        then fThen (gs { guistatePos = spos' }) wi
        else fElse


gdgsIsInputInside :: GUIData -> GUIState -> GUIPos -> GUIShape -> WidgetInput -> 
                     Maybe GUIState
gdgsIsInputInside gd gs pos shape wi = 
    let spos = guistatePos gs 
        ax = guistateScaleX gs
        ay = guistateScaleY gs
        spos' = posScalePlus spos ax ay pos
        shape' = shapeScale ax ay shape
    in  if posIsInside spos' shape' (wiPos wi) ||
           posIsInside spos' shape' (wiPos' wi)
        then Just (gs { guistatePos = spos' })
        else Nothing



--------------------------------------------------------------------------------
--  pos

posPlus :: GUIPos -> GUIPos -> GUIPos 
posPlus (GUIPos x y) (GUIPos x' y') =
    GUIPos (x + x') (y + y')

posScale :: Float -> Float -> GUIPos -> GUIPos
posScale ax ay (GUIPos x y) = 
    GUIPos (ax * x) (ay * y)


posScalePlus :: GUIPos -> Float -> Float -> GUIPos -> GUIPos
posScalePlus (GUIPos x y) ax ay (GUIPos x' y') = 
    GUIPos (x + ax * x') (y + ay * y')


posIsInside :: GUIPos -> GUIShape -> GUIPos -> Bool
posIsInside (GUIPos x y) (GUIShape wth hth) (GUIPos x' y') =
    x <= x' && x' < x + wth &&
    y <= y' && y' < y + hth

posIsAlmostEqual :: GUIPos -> GUIPos -> Bool
posIsAlmostEqual (GUIPos x y) (GUIPos x' y') =
    (x' - x) * (x' - x) + (y' - y) * (y' - y) < epsilon * epsilon
    where
      epsilon = 0.08

posRef :: GUIPos -> GUIPos -> GUIPos
posRef (GUIPos x y) (GUIPos x' y') = 
    GUIPos (x' - x) (y' - y)


shapeScale :: Float -> Float -> GUIShape -> GUIShape
shapeScale ax ay (GUIShape wth hth) =
    GUIShape (ax * wth) (ay * hth)



gdgsPlusPos :: GUIData -> GUIState -> GUIPos -> GUIState
gdgsPlusPos gd gs pos = 
    case posScalePlus (guistatePos gs) (guistateScaleX gs) (guistateScaleY gs) pos of
        pos'  -> gs { guistatePos = pos' }        


gdgsIsPosInside :: GUIData -> GUIState -> GUIShape -> GUIPos -> Bool
gdgsIsPosInside gd gs shape pos =
    posIsInside (guistatePos gs) 
                (shapeScale (guistateScaleX gs) (guistateScaleY gs) shape)
                pos

--------------------------------------------------------------------------------
--  child


{-

posInsideShape :: GUIShape -> GUIPos -> Bool
posInsideShape (GUIShape wth hth) (GUIPos x' y') =
    0 <= x' && x' < wth && 
    0 <= y' && y' < hth


posInsideShape' :: GUIPos -> GUIShape -> GUIPos -> Bool
posInsideShape' (GUIPos x y) (GUIShape wth hth) (GUIPos x' y') =
    let x'' = x' - x
        y'' = y' - y
    in 0 <= x'' && x'' < wth &&
       0 <= y'' && y'' < hth

originInput :: GUIPos -> WidgetInput -> WidgetInput
originInput (GUIPos ox oy) wi =
    case wi of
        WIDrag (GUIPos x y) (GUIPos x' y') t  -> 
                WIDrag (GUIPos (x - ox) (y - oy)) (GUIPos (x' - ox) (y' - oy)) t
        WIDrop (GUIPos x y) (GUIPos x' y') t  -> 
                WIDrop (GUIPos (x - ox) (y - oy)) (GUIPos (x' - ox) (y' - oy)) t
        _                         -> wi


-}
--------------------------------------------------------------------------------
--  containers

{-
beginChildren :: Tick -> [child] -> (Tick -> child -> IO child) -> IO [child]
beginChildren tick children beginChild =
    mapM (beginChild tick) children


eatinputModifyBeforeChild :: parent -> child -> WidgetInput ->
                             (parent -> WidgetInput -> (parent, WidgetInput)) -> 
                             (child -> WidgetInput -> child) ->
                             (parent, child)
eatinputModifyBeforeChild parent child wi parentEat childEat =
    let (parent', wi') = parentEat parent wi
        child' = childEat child wi'

    in (parent', child')


eatinputModifyBeforeChildren :: parent -> [child] -> WidgetInput -> 
                                (parent -> WidgetInput -> (parent, WidgetInput)) ->
                                (child -> WidgetInput -> child) ->
                                (parent, [child])
eatinputModifyBeforeChildren parent children wi parentEat childEat =
    let (parent', wi') = parentEat parent wi
        children' = helper children wi' childEat

    in (parent', children')
    where
        helper (child:childs) wi childEat =
            childEat child wi : helper childs wi childEat
        helper [] wi childEat =
            []
            


eatinputModifyBeforeEachChild :: parent -> [child] -> WidgetInput ->
                                 (parent -> WidgetInput -> parent) -> 
                                 (child -> WidgetInput -> WidgetInput) ->
                                 (child -> WidgetInput -> child) ->
                                 (parent, [child])
eatinputModifyBeforeEachChild parent children wi parentEat wiModify childEat =
    let parent' = parentEat parent wi
        children' = helper children wi wiModify childEat

    in (parent', children')
    where
        helper (child:childs) wi wiModify childEat =
            childEat child (wiModify child wi) : helper childs wi wiModify childEat
        helper [] wi wiModify childEat =
            []

iterateChildren :: [child] -> a -> 
                   (child -> MEnv' ()) -> 
                   (child -> MEnv' ()) -> 
                   (child -> a -> MEnv' (child, a)) ->
                   MEnv' ([child], a)
iterateChildren children a beginIterate endIterate iterate =
    case children of
        (child:childs) -> do
            beginIterate child
            (child', a') <- iterate child a
            endIterate child

            (childs', a'') <- iterateChildren childs a' beginIterate endIterate iterate
            return (child':childs', a'')

        []              ->
            return ([], a)

-}
