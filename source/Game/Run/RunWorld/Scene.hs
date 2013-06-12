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
module Game.Run.RunWorld.Scene
  (
    Scene (..),
    makeScene,

    makeSceneProj3D,
    makeSceneProj2D,

    module Game.Data.Shape,

  ) where

import MyPrelude
import Game
import Game.Data.Shape

import OpenGL
import OpenGL.Helpers


data Scene =
    Scene
    {
        sceneWth :: !UInt,
        sceneHth :: !UInt,
        sceneShape :: !Shape,
        sceneProj3D :: !Mat4,
        sceneProj2D :: !Mat4

    }



-- | make a Scene from size
makeScene :: UInt -> UInt -> Scene
makeScene wth hth =
    let shape = shapeOfSize wth hth 
    in  Scene
        {
            sceneWth = wth,
            sceneHth = hth,
            sceneShape = shape,
            sceneProj3D = makeSceneProj3D (fI wth) (fI hth),
            sceneProj2D = makeSceneProj2D (shapeWth shape) (shapeHth shape)
        }



makeSceneProj3D :: Float -> Float -> Mat4 
makeSceneProj3D wth hth = 
    mat4Perspective valueFOVY (wth / hth) valueNear valueFar

    where
      valueFOVY = valuePerspectiveFOVY
      valueNear = valuePerspectiveNear
      valueFar = valuePerspectiveFar


makeSceneProj2D :: Float -> Float -> Mat4
makeSceneProj2D wth hth = 
    mat4Ortho2D 0 wth hth 0


