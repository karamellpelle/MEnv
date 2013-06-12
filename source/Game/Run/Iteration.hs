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
module Game.Run.Iteration
  (
    iterationShowFont,

  ) where


import MyPrelude
import Game
import Game.Run.RunWorld

import Font
import OpenGL
import OpenGL.Helpers


--------------------------------------------------------------------------------
--  demonstrate Font


iterationShowFont :: Iteration' RunWorld
iterationShowFont = 
    makeIteration' $ \run -> do

        -- output
        run' <- outputShowFont run
        
        let s = (0.5, 0.3)
        iteration' (iterationShowFont' s) run'



iterationShowFont' :: (Float, Float) -> Iteration' RunWorld
iterationShowFont' s =
    defaultIteration s outputShowFont' $ defaultStep noDo $ \s run b -> do
        
        keysTouchHandlePointTouched (run, b, [iterationShowFont' s]) $ \s' ->
                                    (run, b, [iterationShowFont' s'])




--------------------------------------------------------------------------------
--  output

outputShowFont :: RunWorld -> MEnv' RunWorld
outputShowFont run = do
    io $ putStrLn "iterationShowFont"
    return run

outputShowFont' :: (Float, Float) -> RunWorld -> () -> MEnv' ((Float, Float), RunWorld, ())
outputShowFont' pos@(x, y) run b = do
    -- setup Scene
    run' <- beginRunScene run

    fontshade <- resourceFontShade
    fontdata <- resourceFontData

    io $ do
        glDisable gl_CULL_FACE

        -- draw text to Screen
        let proj2D = sceneProj2D $ runScene run'
        fontShade fontshade 1.0 proj2D
        fontDrawDefault fontshade fontdata (valueFontSize * shapeWth (sceneShape (runScene run))) valueFontColor
        fontDraw2DCentered fontshade fontdata x y valueFontText

        glEnable gl_CULL_FACE

        return (pos, run', b) 
    
    where
      valueFontSize = 0.02
      valueFontColor = makeFontColorFloat 1.0 1.0 1.0 1.0
      valueFontText = "Haskell inside!"



beginRunScene :: RunWorld -> MEnv' RunWorld
beginRunScene run = do
    (wth, hth) <- screenSize
    fbo <- screenFBO
    io $ do
        -- setup GL
        glBindFramebuffer gl_FRAMEBUFFER fbo
        glViewport 0 0 (fI wth) (fI hth)
        glClear $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT .|. gl_STENCIL_BUFFER_BIT
       
        -- update Scene
        return run { runScene = makeScene wth hth }

