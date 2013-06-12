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
module Main.IOS
  (
    main',

  ) where

import MyPrelude
import File

import Game
import Game.Run

import OpenGL
import OpenGL.Helpers
import OpenAL
import OpenAL.Helpers




main' :: IO ()
main' = do
   
    -- define MEnv
    let init = MEnvInit
               {
                  initScreenOrientations = [  OrientationLandscapeLeft, OrientationLandscapeRight ],
                  initScreenMultisample = 4,
                  initScreenRate = 0,
                  initSoundSampleRate = 22050,
                  initKeysAcclGyroRate = 0.1
               }
   

    -- run MEnv!
    let a = ()
    c <- runMEnvIOS init loadGameData unloadGameData 
                    begin iterate end a
    return ()

    where
      begin a = do
          
          -- OpenGL and OpenAL --
          io $ do
              -- OpenGL --
              glClearColor 0 0 0 0
              glDisable gl_STENCIL_TEST
              glClearStencil 0
              
              -- we represent colors as premultiplied colors
              glEnable gl_BLEND
              glBlendEquationSeparate gl_FUNC_ADD 
                                      gl_FUNC_ADD
              glBlendFuncSeparate gl_ONE gl_ONE_MINUS_SRC_ALPHA
                                  gl_ONE gl_ONE_MINUS_SRC_ALPHA
              
              glDepthMask gl_TRUE
              glDepthFunc gl_LEQUAL
              glEnable gl_DEPTH_TEST
              glDisable gl_DITHER -- ??

              -- OpenAL --
              alDistanceModel al_INVERSE_DISTANCE
              -- doppler, speed of sound, ...


          -- we want to play this game with a local player (if possible)
          playersAuthenticateLocalPlayer

          -- if first run, create folders and files for dynamic data
          createDynamicData

          -- create RunWorld
          run <- makeRunWorld

          -- run game
          return (run, (), [iterationShowFont])


      iterate abs@(a, b, stack) = do
          iterateABStack a b stack


      end abs@(run, b, stack) = do
          return abs



