{-# Language GeneralizedNewtypeDeriving #-}

module RealEnv where

import System.IO.Unsafe
import Data.IORef
import EventNetwork hiding (Emits(..), EmitState(..), runEm,poll)
import qualified EventNetwork as En
import Control.Monad.IO.Class
import Once
import Control.Monad.Fix
import System.CPUTime

newtype Events a   = E {getE :: ENode a }
newtype Now a = B (Obs a) deriving (Functor,Applicative, Monad,MonadFix)
newtype Pb a = Pb (IO a) deriving (Functor,Applicative, Monad,MonadFix)

data Emits a = Await  [Ex Events] (Now (EmitState a))
data EmitState a = Commit (Maybe a) (Emits a)

translateAwaits :: Emits a -> En.Emits a
translateAwaits (Await l (B b)) =
  En.Await (map (\(Ex (E x)) -> Ex x) l) (transmore <$> b)
  where transmore (Commit m e) = En.Commit m (translateAwaits e)

-- booooooo!! Global variable!
globalEnv :: IORef (Maybe Env)
{-# NOINLINE globalEnv #-}
globalEnv = unsafePerformIO $ newIORef Nothing

liftPb :: Pb a -> Now a
liftPb (Pb b) = B (liftIO b)

isNow :: Events a -> Now (Maybe a)
isNow (E a) = B (checkNode a)

runEm :: Emits a -> Pb (Events a)
runEm a = Pb $
  do Just env <- readIORef globalEnv
     E <$> En.runEmits env (translateAwaits a)

runEmLazy :: Emits a -> Pb (Events a)
runEmLazy a = Pb $
  do Just env <- readIORef globalEnv
     E <$> En.runEmitsIndirect env (translateAwaits a)

hold :: a -> Events a -> Pb (Pb a)
hold i e = Pb $
      do Just env <- readIORef globalEnv
         r <- newTRef env i (getE e)
         return $ Pb $ readTRef r

unsafeRunPb :: Pb a -> a
unsafeRunPb (Pb a) = unsafePerformIO a

unsafePlan :: Events (NowIO a) -> Now (Events a)
unsafePlan e = B $ case plan e of
                    N x -> x


newtype NowIO a = N (Obs a) deriving (Functor,Applicative, Monad,MonadFix)

liftBehavior :: Now a -> NowIO a
liftBehavior (B b) = N b

instance MonadIO NowIO where
  liftIO m = N $ liftIO m

-- booooooo!! Global variable!
strongRefs :: IORef [Ex Events]
{-# NOINLINE strongRefs #-}
strongRefs = unsafePerformIO $ newIORef []

plan :: Events (NowIO a) -> NowIO (Events a)
plan ~(E e) = N $ liftIO $
  do Just env <- readIORef globalEnv
     res <- E <$> En.runEmitsIndirect env loop
     modifyIORef' strongRefs (Ex res :)
     return res where
  loop = En.Await [Ex e] $
    do
       Just (N n) <- checkNode e
       v <- n
       pure $ En.Commit (Just v) loop

callback :: NowIO (Events a, a -> IO ())
callback = N $ liftIO $
  do Just env <- readIORef globalEnv
     (e,cb) <- newBaseE env
     return (E e,cb)



runFRP :: NowIO () -> IO (IO ())
runFRP (N n) =
   do Nothing <- readIORef globalEnv
      env <- newEnv
      writeIORef globalEnv (Just env)
      runObsMDirect env n
      iteration env
      return (iteration env)

unsafeEndOfRound :: IO () -> NowIO ()
unsafeEndOfRound m = N $ liftIO $
  do Just env <- readIORef globalEnv
     addEndOfRound env m

unsafePoll :: IO a -> Now a
unsafePoll m = unsafeRunPb $ Pb $
       do Just env <- readIORef globalEnv
          i <- En.poll env m
          return (B (liftIO i))

-- Time is in seconds, represented as a double.
type Time = Double

{-# NOINLINE timeNow #-}
timeNow :: Now Time
timeNow = unsafePoll $
       do picoSecs <- getCPUTime
          let time = (fromIntegral picoSecs) / 1.0e12
          return time
