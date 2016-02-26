module WxFRP(module AllReal, runWXFrp, wxCb,getIdle, setSource0, setSource, setSourceAttr, setSink) where

import Data.IORef
import AllReal 
import System.IO.Unsafe
import Graphics.UI.WX hiding (prev)
import Control.Monad.IO.Class
import System.CPUTime





-- booooooo!! Global variable!
doIteration :: IORef (IO ())
{-# NOINLINE doIteration #-}
doIteration = unsafePerformIO $ newIORef (return ())

runWXFrp :: Now () -> IO ()
runWXFrp n = start $ 
  do it <- runFRP n
     writeIORef doIteration it

wxCb :: Now (Events a, a -> IO ())
wxCb = do (e,cb) <- callback
          return (e,mod cb) where
  mod cb x = do cb x
                iteration <- readIORef doIteration
                iteration
                return ()

-- Time is in seconds, represented as a double.
type Time = Double

getIdle :: Reactive w => w -> Now (Events Double) 
getIdle w = 
  do (e,cb) <- wxCb
     liftIO $ set w [on idle := (getTime >>= cb) >> return True] 
     return e
  where -- Get the current Time
   getTime :: IO Time
   getTime
     = do picoSecs <- getCPUTime
          let time = (fromIntegral picoSecs) / 1.0e12
          return time

setSourceIdle :: Event w (IO Bool) -> w -> Now (Events ())
setSourceIdle ev w = 
  do (e,cb) <- wxCb
     liftIO $ set w [on ev := cb () >> return True] 
     return e

setSource0 :: Event w (IO ()) -> w -> Now (Events ())
setSource0 ev w = 
  do (e,cb) <- wxCb
     liftIO $ set w [on ev := cb ()] 
     return e

setSourceAttr :: Event w (IO ()) -> Attr w a -> w -> Now (Events a)
setSourceAttr ev a w = 
  do (e,cb) <- wxCb
     liftIO $ set w [on ev := get w a >>= cb ] 
     return e

setSource :: Event w (a -> IO ()) -> w -> Now (Events a)
setSource ev w = 
  do (e,cb) <- wxCb
     liftIO $ set w [on ev := cb] 
     return e

setSink :: Attr w a -> Step a -> w -> Now ()
setSink attr s w = 
  do unsafeEndOfRound $ unsafePrevIO s >>= update 
     plan (liftIO . update <$> updates s)
     return () where
  update x = set w [attr := x] 
          
