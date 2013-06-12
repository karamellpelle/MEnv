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
module GUI.Widget.TextWidget
(
  TextWidget,

  makeTextWidget,
  makeTextWidgetSplit,

  -- tmp:
  outputTextWidget,

) where


import MyPrelude
import GUI.Widget
import GUI.Widget.Output



data TextWidget a =
  TextWidget
  {
      -- textCentered :: !Bool,
      textShape :: !GUIShape,
      textLines :: ![String]
  }


instance Widget TextWidget where
  widgetShape = \gd gs w -> textShape w
  widgetBegin = \gd gs w -> return w
  widgetEatInput = \gd gs w wi -> w
  widgetIterate = textIterate



textIterate :: GUIData -> GUIState -> TextWidget a -> a -> IO (TextWidget a, a)
textIterate gd gs text a = do
  outputTextWidget gd gs text
  return (text, a)



outputTextWidget :: GUIData -> GUIState -> TextWidget a -> IO ()
outputTextWidget gd gs text = do
{-
  -- draw background
  gs' <- useFillTexMid gd gs
  useNoStencil gd gs'
  draw4ShapeNoTex gd gs' $ textShape text
-}
  beginNoDepth gd gs
  
  -- draw texts, stacked downwards
  drawTexts gd gs $ (flip zip) (textLines text) 
                  $ map (\y -> (GUIPos 0.0 y))
                  $ let dy = guidataFontSize gd
                    in  iterate (+ dy) 0.0
  
  endNoDepth gd gs


--------------------------------------------------------------------------------
--  make

-- fixme: hth = 2.0 * unit (or gamedataHth)
-- | make TextWidget from lines.
makeTextWidget :: GUIData -> [String] -> TextWidget a
makeTextWidget gd lines =
  let size = guidataFontSize gd
      charWth = fontdataCharWth (guidataFontData gd) size
      charHth = fontdataCharHth (guidataFontData gd) size
      wth = charWth * fI (foldl max 0 (map length lines))
      hth = charHth * fI (length lines)

  in  TextWidget
      {
          textShape = GUIShape wth hth,
          textLines = lines
      }

-- | make TextWidget from lines, which are "readably" splitted from 'str' 
--   at character width or newline.
makeTextWidgetSplit :: GUIData -> UInt -> String -> TextWidget a
makeTextWidgetSplit gd width str =
  makeTextWidget gd $ splitReadable width str


splitReadable :: UInt -> String -> [String]
splitReadable wth str = 
  concatMap (splitLine wth) $ lines str


splitLine :: UInt -> String -> [String]
splitLine linewth str =
  helper linewth "" $ myWords str
  where
    helper wth line [] = 
      case line of
          []    -> []
          _     -> [line]
    helper wth line (word:words) =
      let wordLen = fI $ length word
      in  if wordLen <= wth 
          -- add word to current line 
          then helper (wth - wordLen) (line ++ word) words
            -- add word to next line
            else case line of
                []    -> helper' wth word words
                line  -> line : helper' linewth word words

    helper' wth word words =
      case helper'' wth word of
          (0, ws, ws')  -> ws : helper' linewth ws' words
          (wth', ws, _) -> helper wth' ws words

    helper'' wth word =
      if wth == 0 
        then (wth, [], word)
        else case word of
            []      -> (wth, [], word)
            (w:ws)  -> let (wth', ws', ws'') = helper'' (wth - 1) ws
                       in  (wth', w:ws', ws'')

    myWords as =
        case as of
            []       -> []
            (a:as')  -> if myIsSpace a 
                        then [a] : myWords as'
                        else let (as', as'') = break myIsSpace as
                             in  as' : myWords as''

    myIsSpace ' ' = True
    myIsSpace _   = False


