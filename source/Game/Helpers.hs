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
module Game.Helpers
  (
    createDynamicData,

    resourceGameData,
    resourceFontShade,
    resourceFontData,
    resourceGUIShade,
    resourceGUIData,

    iterateGUI,
    iterateGUINoInput,

  ) where


import MyPrelude
import Data.Char
import File
import GUI
import Font

import Game.MEnv
import Game.Values

import OpenGL
import OpenGL.Helpers



--------------------------------------------------------------------------------
--  

-- | create dynamic data for application, if not present
createDynamicData :: MEnv' ()
createDynamicData = io $ do

    -- directories
    --createDirectory ...

    -- files
    --copyStaticFile ... 

    return ()

    where
      createDirectory p = do
          fileDynamicData p >>= createDirectoryIfMissing True
      
      copyStaticFile p = do

          path <- fileDynamicData p
          exists <- doesFileExist path 
          unless exists $ do
              path' <- fileStaticData p
              copyFile path' path



--------------------------------------------------------------------------------
--  resources

resourceGameData :: MEnv' GameData
resourceGameData =
    resourceGet

resourceFontShade :: MEnv' FontShade
resourceFontShade =
    fmap gamedataFontShade resourceGet

resourceFontData :: MEnv' FontData
resourceFontData =
    fmap gamedataFontData resourceGet

resourceGUIShade :: MEnv' GUIShade
resourceGUIShade = 
    fmap gamedataGUIShade resourceGet

resourceGUIData :: MEnv' GUIData
resourceGUIData = 
    fmap gamedataGUIData resourceGet

--------------------------------------------------------------------------------
--  GUI 


-- | use MEnv to perform GUI
iterateGUI :: Widget w => Float -> w a -> a -> MEnv' (w a, a)
iterateGUI alpha w a = do
    gamedata <- resourceGameData
    let sh = gamedataGUIShade gamedata
        gd = gamedataGUIData gamedata

    (wth, hth) <- screenShape

    -- query input
    input <- queryInput
    
    -- query tick
    tick <- tickGet

    io $ do
        -- shade
        projmodv <- guiShade sh 1.0 wth hth

        -- iterate. enbles gl_DEPTH_TEST, disables gl_CULL_FACE
        (w', a') <- guiIterate sh gd projmodv alpha wth hth input tick w a

        glEnable gl_CULL_FACE
        return (w', a')

    where
      -- fixme: 2 fingers touching causes no WIDrop input. is this wanted behaviour?
      --        (cf. onNewNumber/onNewValue/...). maybe create WICancelled?
      queryInput = do
          let input = []
          input' <- keysTouchHandlePointDrag input $ \ticks (x, y) (x', y') -> 
              let pos = GUIPos x y
                  pos' = GUIPos x' y'
              in  (WidgetInput pos pos' ticks WIDrag : input)
          input'' <- keysTouchHandlePointDrop input' $ \ticks (x, y) (x', y') ->
              let pos = GUIPos x y
                  pos' = GUIPos x' y'
              in  (WidgetInput pos pos' ticks WIDrop : input')
          return input''


-- | use MEnv to perform GUI, no input
iterateGUINoInput :: Widget w => Float -> w a -> a -> MEnv' (w a, a)
iterateGUINoInput alpha w a = do
    gamedata <- resourceGameData
    let sh = gamedataGUIShade gamedata
        gd = gamedataGUIData gamedata

    (wth, hth) <- screenShape

    -- query tick
    tick <- tickGet

    io $ do
        -- shade
        projmodv <- guiShade sh 1.0 wth hth

        -- iterate
        (w', a') <- guiIterate sh gd projmodv alpha wth hth [] tick w a
        
        glEnable gl_CULL_FACE
        return (w', a')


