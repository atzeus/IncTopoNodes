{-# Language Rank2Types,GeneralizedNewtypeDeriving,GADTs #-} 

module EventNetwork(ENode, Emits(..), EmitState(..), newBaseE , runEmits,runEmitsIndirect, addEndOfRound, TRef, newTRef, readTRef, poll, iteration, Env,newEnv, Ex(..), checkNode, Obs,runObsMDirect)  where
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
  queue      :: !(PrioQueue EState),
  endOfRound :: !(IORef [IO ()])
 }



data EState a = EState {
  action     :: !(IO ()),
  curEmit    :: !(Maybe a),
  behav      :: ![Weak (IORef a)]
 }

type ENode = Node EState

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
  checkNow e f =
    do x <- isBefore e n
       if x 
       then readE e >>= loop . f   
       else do setAction n (checkNow e f)
               ensureAfter n e
               insertPQ (queue env) n



data Emits a = Await { waits :: [Ex ENode], cont :: Obs (EmitState a) }
data EmitState a = Commit (Maybe a) (Emits a)

runEmits :: Env -> Emits a -> IO (ENode a)
runEmits env a = 
  do n <- newNode (EState (return ()) Nothing [])
     runEmitsNode True env n a
     return n

runEmitsIndirect :: Env -> Emits a -> IO (ENode a)
runEmitsIndirect env a = 
 do  n <- newNode (EState (return ()) Nothing [])
     runEmitsNode False env n a
     return n

runEmitsNode :: Bool -> Env -> Node EState a ->  Emits a -> IO ()
runEmitsNode b env n a =
  if b 
  then do addEdges [] (waits a)
          setAction n (loop a)
          insertPQ (queue env) n 
  else setNext [] a where


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

  addEdges [] [] = return ()
  addEdges p [] = mapM_ (\(Ex h) -> removeEdge n h) p
  addEdges [] c = mapM_ (\(Ex h) -> addEdge    n h) c
  addEdges (Ex hp : tp) (Ex h : t) 
     | hp `heqNode` h = addEdges tp t
     | otherwise = do removeEdge n hp
                      addEdge n h
                      addEdges tp t

commit env n v = do setEmit n v
                    addClean n
                    scheduleParents (queue env) n
                     where

  setEmit n a = modifyNode' n (\x -> x {curEmit = a})
  addClean  n = modifyIORef' (endOfRound env) (cleanE n :)
  
addEndOfRound :: Env -> IO () -> IO ()
addEndOfRound env end = 
   modifyIORef' (endOfRound env) (end :)

setAction   n a = modifyNode' n (\x -> x {action = a})

readE n  = curEmit <$> readNode n
risNow n = isJust . curEmit <$> readNode n

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

-- executed 0 or once per iteration
poll :: Env -> IO a -> IO (IO a)
poll env m =  
  do r <- newIORef Nothing
     return (update r) where
  update r =
    do v <- readIORef r
       case v of 
         Just x -> return x
         Nothing -> do v <- m
                       writeIORef r (Just v)
                       addEndOfRound env (writeIORef r Nothing)
                       return v

newtype TRef a = TRef (IORef a)

-- non-strict in node!
newTRef :: Env -> a -> ENode a -> IO (TRef a)
newTRef env a e = 
   do r <- newIORef a 
      addEndOfRound env $ do
         wr <- mkWeakIORef r (return ())
         modifyNode' e (\s -> s {behav = wr : behav s})
         cv <- readE e
         case cv of
          Just x -> writeIORef r x
          Nothing -> return ()
      return (TRef r)

readTRef :: TRef a -> IO a
readTRef (TRef r) = readIORef r


handleQueue :: Env -> IO ()
handleQueue env = 
  do n <- sizeQueue (queue env)
     h <- dequeue (queue env)
     case h of
       Just (ExNode m) -> 
           do a <- action <$> readNode m  
              a 
              handleQueue env
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
newEnv = Env <$> emptyPqueue <*> newIORef []

newBaseE :: Env -> IO (ENode a, a -> IO ())
newBaseE env =
    do n <- newNode (EState (return ()) Nothing [])
       return (n, emit n) where
   emit n x = commit env n (Just x)

