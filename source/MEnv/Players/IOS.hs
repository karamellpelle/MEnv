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
{-# LANGUAGE ForeignFunctionInterface #-}
module MEnv.Players.IOS
  (
    Player (..),

    playersAuthenticateLocalPlayer,
    playersHandleLocalPlayer,
    playersSendAchievement,
    playersSendScore,

  ) where

import MyPrelude
import Foreign
import Foreign.C
import MEnv


data Player =
    Player
    {
        playerID :: String,
        playerAlias :: String
    }


--------------------------------------------------------------------------------
--  

foreign import ccall unsafe "&ios_players_new_local_player" 
    ios_players_new_local_player :: Ptr CUInt

foreign import ccall unsafe "&ios_players_local_player_id_len" 
    ios_players_local_player_id_len :: Ptr CUInt

foreign import ccall unsafe "&ios_players_local_player_alias_len" 
    ios_players_local_player_alias_len :: Ptr CUInt

foreign import ccall unsafe "&ios_players_local_player_id" 
    ios_players_local_player_id :: (Ptr (Ptr CChar))

foreign import ccall unsafe "&ios_players_local_player_alias" 
    ios_players_local_player_alias :: (Ptr (Ptr CChar))


-- | try to play as a local player. the local player is handled with 
--   playersHandleLocalPlayer, and may be called multiple times with
--   different players during the lifetime of calling program
playersAuthenticateLocalPlayer :: MEnv res ()
playersAuthenticateLocalPlayer = io $ 
    ios_playersAuthenticateLocalPlayer

foreign import ccall unsafe "ios_playersAuthenticateLocalPlayer" 
    ios_playersAuthenticateLocalPlayer :: IO ()



-- | play with given local player.
--
--          "Never make assumptions about the format or length of player identifier 
--          strings" - iOS Documentation
--
--          :)
playersHandleLocalPlayer :: a -> (Player -> a) -> MEnv res a
playersHandleLocalPlayer a f = io $ do
    peek ios_players_new_local_player >>= \v -> case v of
        0     -> return a
        _     -> do
            id <- peek ios_players_local_player_id
            alias <- peek ios_players_local_player_alias
            idLen <- peek ios_players_local_player_id_len
            aliasLen <- peek ios_players_local_player_alias_len

            id' <- peekCStringLen (id, fI idLen)
            alias' <- peekCStringLen (alias, fI aliasLen)
            poke ios_players_new_local_player 0
            return $ f (Player id' alias')



playersLocalPlayer :: MEnv res (Maybe Player)
playersLocalPlayer = io $ do
    peek ios_players_local_player_id >>= \id -> if id == nullPtr 
        then return Nothing
        else do     
            --id <- peek ios_players_local_player_id
            alias <- peek ios_players_local_player_alias
            idLen <- peek ios_players_local_player_id_len
            aliasLen <- peek ios_players_local_player_alias_len

            id' <- peekCStringLen (id, fI idLen)
            alias' <- peekCStringLen (alias, fI aliasLen)
            return (Just $ Player id' alias')



-- | send value 'alpha' (in [0, 1]) to achievement 'ach', for local player
playersSendAchievement :: String -> Float -> MEnv res ()
playersSendAchievement ach alpha = io $ 
    withCString ach $ \ptrAch -> 
        ios_playersSendAchievement ptrAch (rTF alpha)

foreign import ccall unsafe "ios_playersSendAchievement" 
    ios_playersSendAchievement :: CString -> CFloat -> IO ()




-- | send score value 'score' of category 'cat', for local player
playersSendScore :: String -> Int64 -> MEnv res ()
playersSendScore cat score = io $ 
    withCString cat $ \ptrCat -> 
        ios_playersSendScore ptrCat (fI score)

-- from types.h : typedef long long int64_t; 
foreign import ccall unsafe "ios_playersSendScore" 
    ios_playersSendScore :: CString -> CLLong -> IO () 


