{-# Language FlexibleInstances,ScopedTypeVariables, GADTs,GeneralizedNewtypeDeriving, ViewPatterns, TupleSections #-} 

module EventNetwork(E, newBaseE , runEmits, TRef, newTRef, readTRef, iteration, newEnv)  where
import FList 
import IncTopoSort
import System.Mem.Weak
import Data.IORef
import Data.Maybe
import System.IO.Unsafe

data Env = Env {
  queue    :: !(PrioQueue EState),
  clean    :: !(IORef [Ex E]) 
 }

data Await a where
  Await  :: FList E l -> (FList Maybe l -> IO (EmitState a)) -> Await a

data EmitState a = WaitMore (Await a)
                 | Commit (Maybe a) (Await a)

type Emits a = IO (Await a)

data EState a = EState {
  action     :: !(IO ()),
  curEmit    :: !(Maybe a),
  visited    :: Bool,
  behav      :: ![Weak (IORef a)]
 }

data E a = E (Node EState a)


runEmits :: Env -> Emits a -> IO (E a)
runEmits env e = 
  do n <- newNode (EState (return ()) Nothing False [])
     a <- e
     runAwait n a
     return (E n) where

  runAwait n a =
    do setupAwait n a  
       insertPQ (queue env) n

  setupAwait n a@(Await l f) =
    do addEdges n l
       setAction n (reactAwait n a)

  reactAwait n (Await l f) =
    do ss <- getStates l
       let anyEmit = any id $ toList isJust ss
       if not anyEmit
       then return ()
       else do removeEdges n l
               x <- f ss
               case x of
                WaitMore c -> runAwait n c
                Commit v m -> do commit env n v
                                 setupAwait n m                                                                   

  addEdges    n l = sequence $ toList (\(E to) -> addEdge n to) l
  getStates   l   = mapfM readE l
  removeEdges n l = sequence $ toList (\(E to) -> removeEdge n to) l
  ifFirst     n a = do x <- visited <$> readNode n
                       if x then return () else a
  setAction   n a = modifyNode' n (\x -> x {action = ifFirst n a})

  readE (E n) = curEmit <$> readNode n

commit env n v = do setEmit n v
                    addClean n
                    scheduleParents (queue env) n
  where setEmit n a = modifyNode' n (\x -> x {curEmit = a, visited = True})
        addClean  n = modifyIORef' (clean env) (Ex (E n) :)

newtype TRef a = TRef (IORef a)

newTRef :: a -> E a -> IO (TRef a)
newTRef a (E e) = 
   do r <- newIORef a 
      wr <- mkWeakIORef r (return ())
      modifyNode' e (\s -> s {behav = wr : behav s})
      return (TRef r)

readTRef :: TRef a -> IO a
readTRef (TRef r) = readIORef r

cleanUp :: Env -> IO ()
cleanUp e = 
    do s <- readIORef (clean e)
       mapM_ cleanE s
       writeIORef (clean e) [] where

 cleanE (Ex (E n))  =
   do es <- readNode n    
      writeNode n (es {curEmit = Nothing, visited = False})              
      case curEmit es of
       Just x -> setRefs n x
       _      -> return () 
  
 setRefs n x  = 
   do es <- readNode n    
      bm <- mapM deRefWeak (behav es)
      mapM (\b -> writeIORef b x) (catMaybes bm)
      let b' = catMaybes $ zipWith (\x y -> x <$ y) (behav es) bm
      writeNode n (es {behav = b' } )


handleQueue :: Env -> IO ()
handleQueue env = 
  do h <- dequeue (queue env)
     case h of
       Just (ExNode m) -> 
           do a <- action <$> readNode m  
              a 
       Nothing -> return ()

iteration :: Env -> IO ()
iteration env = do handleQueue env
                   cleanUp env

newEnv :: IO Env 
newEnv = Env <$> emptyPqueue <*> newIORef []

newBaseE :: Env -> IO (E a, a -> IO ())
newBaseE env =
    do n <- newNode (EState (return ()) Nothing False [])
       return (E n, emit n) where
   emit n x = commit env n (Just x)

