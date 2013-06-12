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
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Game.Helpers.World
  (

    --  fixme: clean up all these
    handleAllEvents,
    handleEventsBreak,
    handleOneEvent,
    breakEvents,
    continueEvents,

    handleAllEventsM,
    handleAllEventsM_,
    handleEventsBreakM,
    breakEventsM,
    continueEventsM,
    handleOneEventM,
    handleEventM,
    nextEventM,

  ) where


import Game.World


handleAllEvents :: World w e => w -> a -> (a -> e -> a) -> a
handleAllEvents w a f = 
    helper a (worldAllEvents w)
    where
      helper a (e:es) =
          helper (f a e) es
      helper a [] =
          a

handleEventsBreak :: World w e => w -> a -> (a -> e -> (Bool, a)) -> a
handleEventsBreak w a f =
    helper a (worldAllEvents w)
    where
      helper a (e:es) =
        case f a e of
            (False, a') -> helper a' es
            (True, a')  -> a'
      helper a [] =
          a

breakEvents :: a -> (Bool, a)
breakEvents a =
    (True, a)

continueEvents :: a -> (Bool, a)
continueEvents a =
    (False, a)


handleOneEvent :: World w e => w -> a -> (e -> Maybe a) -> a
handleOneEvent w end f =
    helper (worldAllEvents w)
    where
        helper (e:es) =
            case f e of
                Nothing   -> helper es
                Just a    -> a
        helper [] =
            end


handleAllEventsM :: (Monad m, World w e) => w -> a -> (a -> e -> m a) -> m a
handleAllEventsM w a f = 
    helper a (worldAllEvents w)
    where
      helper a (e:es) = do
          a' <- f a e
          helper a' es
      helper a [] =
          return a


handleAllEventsM_ :: (Monad m, World w e) => w -> (e -> m ()) -> m ()
handleAllEventsM_ w f =
    mapM_ f (worldAllEvents w)


handleEventsBreakM :: (Monad m, World w e) => w -> a -> (a -> e -> m (Bool, a)) -> m a
handleEventsBreakM w a f =
    helper a (worldAllEvents w)
    where
      helper a (e:es) = do
          (break, a') <- f a e
          case break of
              False   -> helper a' es
              True    -> return a'
      helper a [] = 
          return a


breakEventsM :: Monad m => a -> m (Bool, a)
breakEventsM a = do
    return (True, a)


continueEventsM :: Monad m => a -> m (Bool, a)
continueEventsM a = do
    return (False, a)


handleOneEventM :: (Monad m, World w e) => w -> m a -> (e -> m (Maybe a)) -> m a
handleOneEventM w end f =
    helper (worldAllEvents w)
    where
        helper (e:es) =
            f e >>= \maybeA -> case maybeA of
                Nothing   -> helper es
                Just a    -> return a
        helper [] =
            end


handleEventM :: Monad m => m a -> m (Maybe a)
handleEventM ma =
    ma >>= \a -> return $ Just a


nextEventM :: Monad m => m (Maybe a)
nextEventM =
    return $ Nothing
