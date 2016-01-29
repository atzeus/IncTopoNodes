{-# Language GADTs #-} 

module EventNetwork(ENode, Emits(..), EmitState(..), newBaseE , runEmits, TRef, newTRef, readTRef, iteration, Env,newEnv, Ex(..))  where
import FList 
import IncTopoSort
import System.Mem.Weak
import Data.IORef
import Data.Maybe
import Data.Unique

data Env = Env {
  uuid     :: Unique,
  queue    :: !(PrioQueue EState),
  clean    :: !(IORef [Ex ENode]) 
 }

instance Eq Env where
  l == r = uuid l == uuid r

data Emits a where
  Await  :: FList ENode l -> (FList Maybe l -> IO (EmitState a)) -> Emits a

data EmitState a = WaitMore (Emits a)
                 | Commit (Maybe a) (Emits a)


data EState a = EState {
  action     :: !(IO ()),
  curEmit    :: !(Maybe a),
  visited    :: Bool,
  behav      :: ![Weak (IORef a)]
 }

data ENode a = E !(Unique) !(Node EState a)


runEmits :: Env -> Emits a -> IO (ENode a)
runEmits env e = 
  do n <- newNode (EState (return ()) Nothing False [])
     runAwait n e
     return (E (uuid env) n) where

  runAwait n a =
    do setupAwait n a  
       insertPQ (queue env) n

  setupAwait n a@(Await l f) =
    do if checkUid l
       then do addEdges n l
               setAction n (reactAwait n a)
       else error "Event nodes not from same enviroment!"

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


  addEdges    n l = sequence $ toList (\(E _ to) -> addEdge n to) l
  checkUid :: FList ENode l -> Bool
  checkUid    l   = all id $ toList (\(E u _) -> uuid env == u) l
  getStates   l   = mapfM readE l
  removeEdges n l = sequence $ toList (\(E _ to) -> removeEdge n to) l
  ifFirst     n a = do x <- visited <$> readNode n
                       if x then return () else a
  setAction   n a = modifyNode' n (\x -> x {action = ifFirst n a})

  readE (E _ n) = curEmit <$> readNode n

commit env n v = do setEmit n v
                    addClean n
                    scheduleParents (queue env) n
  where setEmit n a = modifyNode' n (\x -> x {curEmit = a, visited = True})
        addClean  n = modifyIORef' (clean env) (Ex (E (uuid env) n) :)

newtype TRef a = TRef (IORef a)

newTRef :: a -> ENode a -> IO (TRef a)
newTRef a (E _ e) = 
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

 cleanE (Ex (E _ n))  =
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
newEnv = Env <$> newUnique <*> emptyPqueue <*> newIORef []

newBaseE :: Env -> IO (ENode a, a -> IO ())
newBaseE env =
    do n <- newNode (EState (return ()) Nothing False [])
       return (E (uuid env) n, emit n) where
   emit n x = commit env n (Just x)

