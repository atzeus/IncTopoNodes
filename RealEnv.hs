{-# Language GeneralizedNewtypeDeriving #-} 

module RealEnv where

import System.IO.Unsafe
import Data.IORef
import EventNetwork hiding (Emits(..), EmitState(..), runEm)
import qualified EventNetwork as En
import Control.Monad.IO.Class
import Once
import Control.Monad.Fix

newtype Events a   = E (ENode a)
newtype Behavior a = B (Obs a) deriving (Functor,Applicative, Monad,MonadFix)
newtype Pb a = Pb (IO a) deriving (Functor,Applicative, Monad,MonadFix)

data Emits a = Await  [Ex Events] (Behavior (EmitState a)) 
data EmitState a = Commit (Maybe a) (Emits a)

translateAwaits :: Emits a -> En.Emits a
translateAwaits (Await l (B b)) = 
  En.Await (map (\(Ex (E x)) -> Ex x) l) (transmore <$> b)
  where transmore (Commit m e) = En.Commit m (translateAwaits e)

-- booooooo!! Global variable!
globalEnv :: IORef (Maybe Env)
{-# NOINLINE globalEnv #-}
globalEnv = unsafePerformIO $ newIORef Nothing

liftPb :: Pb a -> Behavior a
liftPb (Pb b) = B (liftIO b)

isNow :: Events a -> Behavior (Maybe a)
isNow (E a) = B (checkNode a)

runEm :: Emits a -> Pb (Events a)
runEm a = Pb $ 
  do Just env <- readIORef globalEnv
     E <$> En.runEmits env (translateAwaits a)

hold :: a -> Events a -> Pb (Pb a)
hold i (E e) = Pb $ 
      do Just env <- readIORef globalEnv
         r <- newTRef env i e
         return $ Pb $ readTRef r

unsafeRunPb :: Pb a -> a
unsafeRunPb (Pb a) = unsafePerformIO a


newtype Now a = N (Obs a) deriving (Functor,Applicative, Monad,MonadFix)

liftBehavior :: Behavior a -> Now a 
liftBehavior (B b) = N b

instance MonadIO Now where
  liftIO m = N $ liftIO m

-- booooooo!! Global variable!
strongRefs :: IORef [Ex Events]
{-# NOINLINE strongRefs #-}
strongRefs = unsafePerformIO $ newIORef []

plan :: Events (Now a) -> Now (Events a)
plan (E e) = N $ liftIO $
  do Just env <- readIORef globalEnv
     res <- E <$> En.runEmits env loop
     modifyIORef' strongRefs (Ex res :)
     return res where
  loop = En.Await [Ex e] $
    do Just (N n) <- checkNode e
       v <- n
       pure $ En.Commit (Just v) loop

callback :: Now (Events a, a -> IO ())
callback = N $ liftIO $
  do Just env <- readIORef globalEnv
     (e,cb) <- newBaseE env
     return (E e,cb)

runFRP :: Now () -> IO (IO ())
runFRP (N n) = 
   do Nothing <- readIORef globalEnv
      env <- newEnv 
      writeIORef globalEnv (Just env)
      runObsMDirect env n
      return (iteration env)

