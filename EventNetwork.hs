{-# Language Rank2Types,GeneralizedNewtypeDeriving,GADTs #-} 

module EventNetwork(ENode, Emits(..), EmitState(..), newBaseE , runEmits, TRef, newTRef, readTRef, iteration, Env,newEnv, Ex(..), checkNode, Obs,runObsMDirect)  where
import FList 
import IncTopoSort
import System.Mem.Weak
import Data.IORef
import Data.Maybe
import Data.Unique
import OpMonad
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Fix
import System.IO.Unsafe

data Env = Env {
  uuid       :: Unique,
  queue      :: !(PrioQueue EState),
  endOfRound :: !(IORef [IO ()])
 }

instance Eq Env where
  l == r = uuid l == uuid r

data EState a = EState {
  action     :: !(IO ()),
  curEmit    :: !(Maybe a),
  behav      :: ![Weak (IORef a)]
 }

data ENode a = E { uid :: !(Unique), getNode ::  !(Node EState a) }

data ObsF a where
  IsNow :: ENode a -> ObsF (Maybe a)
  LiftIO :: IO a -> ObsF a

type ObsM a = M ObsF a

newtype Obs a = O (ObsM a) deriving (Functor,Applicative,Monad)

instance MonadIO Obs where
  liftIO m = O (op $ LiftIO m)

instance MonadFix Obs where
  mfix f = 
    do r <- liftIO $ newIORef undefined
       x <- f (unsafePerformIO $ readIORef r)
       liftIO $ writeIORef r x
       return x

checkNode :: ENode a -> Obs (Maybe a)
checkNode e = O (op $ IsNow e)

runObsMDirect :: Env -> Obs () -> IO ()
runObsMDirect env m =
  do n <- newNode (EState (return ()) Nothing [])
     runObsM env n m

runObsM :: Env -> Node EState a -> Obs () -> IO ()
runObsM env n (O m) = loop m where 
  loop m =
   case viewM m of
     Pure _ -> setAction n (return ())
     Act a f ->
      case a of
        LiftIO i          -> i >>= loop . f
        IsNow e           -> checkNow e f

  checkNow :: ENode a -> (Maybe a -> ObsM ()) -> IO ()
  checkNow ev@(E _ e) f =
    do x <- isBefore e n
       if x 
       then readE ev >>= loop . f   
       else do setAction n (checkNow ev f)
               ensureAfter n e
               insertPQ (queue env) n

data Emits a = Await { waits :: [Ex ENode], cont :: Obs (EmitState a) }
data EmitState a = Commit (Maybe a) (Emits a)

runEmits :: Env -> Emits a -> IO (ENode a)
runEmits env a = 
  do n <- newNode (EState (return ()) Nothing [])
     runEmitsNode env n a
     return (E (uuid env) n)

runEmitsNode :: Env -> Node EState a ->  Emits a -> IO ()
runEmitsNode env n a =
  do addEdges [] (waits a)
     setAction n (loop a)
     insertPQ (queue env) n where

  loop (Await [] _) = return ()
  loop (Await lis co) = 
     do b <- checkIsNow lis
        if b
        then runObsM env n $ 
             do Commit e a <- co
                liftIO $ setNext lis a >> commit env n e   
        else return ()

  checkIsNow lis = or <$> mapM (\(Ex m) -> risNow m) lis

  setNext prev a = addEndOfRound env $
    do addEdges prev (waits a)
       setAction n (loop a)

  addEdges p [] = mapM_ (\(Ex (E _ h)) -> removeEdge n h) p
  addEdges [] c = mapM_ (\(Ex (E _ h)) -> addEdge    n h) c
  addEdges (Ex (E _ hp) : tp) (Ex (E _ h) : t) 
     | hp `heqNode` h = addEdges tp t
     | otherwise = do removeEdge n hp
                      addEdge n h
                      addEdges tp t

commit env n v = do setEmit n v
                    addClean n
                    scheduleParents (queue env) n where

  setEmit n a = modifyNode' n (\x -> x {curEmit = a})
  addClean  n = modifyIORef' (endOfRound env) (cleanE n :)
  
addEndOfRound :: Env -> IO () -> IO ()
addEndOfRound env end = 
   modifyIORef' (endOfRound env) (end :)

setAction   n a = modifyNode' n (\x -> x {action = a})

readE (E _ n) = curEmit <$> readNode n
risNow (E _ n) = isJust . curEmit <$> readNode n



cleanE n  =
 do es <- readNode n    
    writeNode n (es {curEmit = Nothing})              
    case curEmit es of
     Just x -> setRefs n x
     _      -> return () 
 where 
  setRefs n x  = 
   do es <- readNode n    
      bm <- mapM deRefWeak (behav es)
      mapM (\b -> writeIORef b x) (catMaybes bm)
      let b' = catMaybes $ zipWith (\x y -> x <$ y) (behav es) bm
      writeNode n (es {behav = b' } )


newtype TRef a = TRef (IORef a)

newTRef :: a -> ENode a -> IO (TRef a)
newTRef a (E _ e) = 
   do r <- newIORef a 
      wr <- mkWeakIORef r (return ())
      modifyNode' e (\s -> s {behav = wr : behav s})
      return (TRef r)

readTRef :: TRef a -> IO a
readTRef (TRef r) = readIORef r


handleQueue :: Env -> IO ()
handleQueue env = 
  do h <- dequeue (queue env)
     case h of
       Just (ExNode m) -> 
           do a <- action <$> readNode m  
              a 
       Nothing -> return ()

endOfRounds :: Env -> IO ()
endOfRounds env = 
  do ends <- readIORef (endOfRound env)
     writeIORef (endOfRound env) []
     sequence_  ends 

iteration :: Env -> IO ()
iteration env = do handleQueue env
                   endOfRounds env

newEnv :: IO Env 
newEnv = Env <$> newUnique <*> emptyPqueue <*> newIORef []

newBaseE :: Env -> IO (ENode a, a -> IO ())
newBaseE env =
    do n <- newNode (EState (return ()) Nothing [])
       return (E (uuid env) n, emit n) where
   emit n x = commit env n (Just x)

