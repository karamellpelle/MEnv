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
module Game.GameData
  (
    GameData (..),

    loadGameData,
    unloadGameData,

  ) where

import MyPrelude
import File
import Font
import GUI

import Game.Values


data GameData =
    GameData
    {
        gamedataFontShade :: !FontShade,              -- ^ shader for Font
        gamedataFontData :: !FontData,                -- ^ default font used in game
        gamedataGUIShade :: !GUIShade,                -- ^ shader for GUI
        gamedataGUIData :: !GUIData                   -- ^ data for GUI
    }



loadGameData :: IO GameData
loadGameData = do

    -- Font --
    -- FontShade
    path <- fileStaticData "Font"
    fontsh <- loadFontShade path

    -- FontData (this is magic)
    path <- fileStaticData "fonts/UbuntuMono"
    fontdata <- makeFontData path 16 8 0.62 0.95 32

    -- GUI --
    -- GUIShade
    path  <- fileStaticData "GUI"
    guish <- loadGUIShade path

    -- GUIData
    path <- fileStaticData "guis/my_guistyle"
    guidata <- loadGUIData fontsh path


    return $  GameData
              {
                  gamedataFontShade = fontsh,
                  gamedataFontData = fontdata,
                  gamedataGUIShade = guish,
                  gamedataGUIData = guidata
              }
 
    where
      loadGUIData fontsh style =
          makeGUIData GUIMake
                      {
                          mkGUIStyle = style,
                          mkGUIFillTexRepeat = 8.0,

                          mkGUIFontShade = fontsh,
                          mkGUIFontColor = makeFontColorGLubyte 0x70 0x00 0xff 0xff,
                          mkGUIFontSize = 0.036
                      }
     



unloadGameData :: GameData -> IO ()
unloadGameData gamedata =
    fixme "unloadGameData"
