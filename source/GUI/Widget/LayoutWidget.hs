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
module GUI.Widget.LayoutWidget
  (
    LayoutWidget,

    makeLayoutWidget,
    makeLayoutWidgetStatic,
 
    module GUI.Widget.ChildWidget,
    module GUI.Widget.LayoutWidget.StaticLayout,

  ) where



import MyPrelude
import GUI.Widget
import GUI.Widget.Output
import GUI.Widget.Helpers
import GUI.Widget.ChildWidget
import GUI.Widget.LayoutWidget.StaticLayout


data LayoutWidget a =
    LayoutWidget
    {
        layoutElements :: ![LayoutElement a],
        layoutShape :: !GUIShape

    }


data LayoutElement a =
    LayoutElement
    {
        lePos :: !GUIPos,
        leChild :: !(ChildWidget a)
    }


instance Widget LayoutWidget where
    widgetShape = \gd gs w -> layoutShape w
    widgetBegin = layoutBegin
    widgetEatInput = layoutEatInput
    widgetIterate = layoutIterate



--------------------------------------------------------------------------------
--  Widget structure



layoutBegin :: GUIData -> GUIState -> LayoutWidget a -> IO (LayoutWidget a)
layoutBegin gd gs layout = do
    let es = layoutElements layout
    es' <- mapESBegin es
    return $ layout { layoutElements = es' }

    where
      mapESBegin es = case es of
          []      -> return []
          (e:es)  -> do
              child' <- widgetBegin gd gs (leChild e)
              es' <- mapESBegin es
              return (e { leChild = child' } : es')


layoutEatInput :: GUIData -> GUIState -> LayoutWidget a -> WidgetInput -> LayoutWidget a
layoutEatInput gd gs layout wi =
    let es' = mapESEatInput (layoutElements layout)
    in  layout { layoutElements = es' }
    
    where
      mapESEatInput es = case es of
          []      -> 
              []
          (e:es)  -> 
              let child = leChild e
                  pos = lePos e
                  shape = widgetShape gd gs child
              in  case gdgsIsInputInside gd gs pos shape wi of
                  Nothing   -> 
                      (e:mapESEatInput es)
                  Just gs'  -> 
                      let child' = widgetEatInput gd gs' child wi
                      in  (e { leChild = child' } : mapESEatInput es)
                      --in  (e { leChild = child' } : es)


layoutIterate :: GUIData -> GUIState -> LayoutWidget a -> a -> IO (LayoutWidget a, a)
layoutIterate gd gs layout a = do

    -- iterate children
    let es = layoutElements layout
    (es', a') <- mapESIterate gd gs es a
    
    return (layout { layoutElements = es' }, a')
    
    where
      mapESIterate gd gs es a = case es of
          []      -> return ([], a)
          (e:es)  -> do
               
              -- iterate child
              gs' <- plusPos gd gs (lePos e)
              (child', a') <- widgetIterate gd gs' (leChild e) a
              resetPos gd gs
            
              -- iterate children
              (es', a'') <- mapESIterate gd gs es a'
              return (e { leChild = child' } : es', a'')
    



--------------------------------------------------------------------------------
--  make 


makeLayoutWidget :: GUIData -> GUIShape -> [(GUIPos, ChildWidget a)] -> LayoutWidget a
makeLayoutWidget gd shape = \pws ->
    LayoutWidget 
    {
        layoutShape = shape,
        layoutElements = map (\(pos, w) -> LayoutElement pos w) pws
    }


makeLayoutWidgetStatic :: StaticLayout layout => GUIData -> layout a -> LayoutWidget a
makeLayoutWidgetStatic gd = \slayout ->
    makeLayoutWidget gd (slayoutShape gd slayout) (slayoutChildren gd slayout)

