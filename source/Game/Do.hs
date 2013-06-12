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
module Game.Do
  (
    defaultDo,
    noBreakModify,
    noDefaultModify,

  ) where


import MyPrelude
import MEnv.Tick
import Game.MEnv
import Game.World


-- | remember to typically always increment worldTick with dt, 
--   else this function hangs!
defaultDo :: World a e => (s -> a -> b -> MEnv' (s, a, b)) ->
                          (MEnv' Tick, Tick -> MEnv' ()) ->
                          (Tick -> s -> a -> b -> MEnv' (s, a, b)) ->
                          (s -> a -> b -> MEnv' (Maybe (s, a, b))) ->
                          (s -> a -> b -> MEnv' (s, a, b)) -> 
                          s -> a -> b -> MEnv' (s, a, b)
defaultDo modify
          (getTick, setTick) 
          stepDT
          breakModify
          defaultModify = \s a b -> do

        -- modify world begin
        (s', a', b') <- modify s a b

        -- ignore too long elaps
        tick <- getTick
        when ( worldTick a' + valueMaxElaps <= tick ) $ do
            setTick ( worldTick a' + valueMaxElaps )
        tick <- getTick

        -- step physics in 'valueDTUnit' portions
        helper tick s' a' b'
        where
          helper tick s a b =
              if worldTick a + valueDTUnit <= tick

                -- take a 'valueDTUnit'-step of physical objects
                then do
                  (s', a', b') <- stepDT valueDTUnit s a b
                  maybeSAB <- breakModify s' a' b'
                  case maybeSAB of
                      Nothing               -> helper tick s' a' b'
                      -- modify world end
                      Just (s'', a'', b'')  -> return (s'', a'', b'')

                -- modify world end
                else do
                  defaultModify s a b



noBreakModify :: s -> a -> b -> MEnv' (Maybe (s, a, b))
noBreakModify s a b =
    return Nothing


noDefaultModify :: s -> a -> b -> MEnv' (s, a, b)
noDefaultModify s a b =
    return (s, a, b)


--------------------------------------------------------------------------------
--  values

valueDTUnit :: Tick
valueDTUnit =
    0.02

valueMaxElaps :: Tick
valueMaxElaps =
    2.0


