module GlossFRP(glossFRP, glossFRPNow) where

import FRPLib
import AllReal
import Graphics.Gloss.Interface.IO.Game
import Data.IORef
import Control.Monad.IO.Class

type Time = Float

type FPS = Int

glossFRP :: Display -> Color -> FPS ->
            (Events Event -> Events Float -> Now (Behavior Picture)) ->
            IO ()
glossFRP d c fps f = glossFRPNow d c fps (\e dt -> liftB $ f e dt)
{-
glossFRPb :: Display -> Color -> FPS ->
            (Events Event -> Events Float -> Now (Now Picture)) ->
            IO ()
glossFRPb d c fps f =
  do glossFRP d c fps $ \e dt ->
        do i <- f e dt
           v <- i
           step v (i <@ dt)
-}
glossFRPNow :: Display -> Color -> FPS ->
            (Events Event -> Events Float -> NowIO (Behavior Picture) ) ->
            IO ()
glossFRPNow d c fps f =
  do pict <- newIORef Blank
     cbs <- newIORef undefined
     update <- runFRP $
                do (e,ecb)   <- callback
                   (dt,dtcb) <- callback
                   liftIO $ writeIORef cbs (ecb,dtcb)
                   picts <- f e dt
                   stepToRef picts pict
     (ecb,dtcb) <- readIORef cbs
     playIO d c fps ()
      (\_ -> readIORef pict)
      (\e _ -> ecb e >> update)
      (\dt _ -> dtcb dt >> update)

stepToRef :: Behavior a -> IORef a -> NowIO ()
stepToRef s r =
  do i <- liftB $ prev s
     liftIO $ writeIORef r i
     plan (liftIO . writeIORef r <$> updates s)
     return ()
