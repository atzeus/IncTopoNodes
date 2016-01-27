{-# Language FlexibleInstances,ScopedTypeVariables, GADTs,GeneralizedNewtypeDeriving, ViewPatterns, TupleSections #-} 

module Impl2(Emits, B, isNow)  where

import IncTopoSort
import System.Mem.Weak
import Data.IORef
import Data.Unique
import Control.Monad.Fix
import Data.Maybe
import System.IO.Unsafe
import Once
import OpMonad

newtype Ndd a = Ndd (Node (EState a))

data EnvState = ES {
  clean    :: ![Ex Ndd]
 }

data Env = Env {
  uid      :: !Unique,
  queue    :: !(PrioQueue (IO ())),
  envState :: !(IORef EnvState) 
 }

instance Eq Env where
  a == b = uid a == uid b


data Bf a where
  IsNow :: E a  -> Bf (Maybe a)
  IO    :: IO a -> Bf a
  Fix   :: (a -> M Bf a) -> Bf a

instance MonadFix (M Bf) where
  mfix f = op (Fix f)

newtype B a = B (M Bf a) deriving (Functor,Applicative,Monad, MonadFix)

runB :: Env -> Node a -> B () -> IO ()
runB env n (B m) = go m where
  go m = case viewM m of
          Pure _ -> return ()
          Act a f -> 
           case a of
            IsNow (E _ ne) ->
               do e <- ne
                  es <- readE e
                  case es of
                    Emit x  -> go (f (Just x))
                    Skip    -> go (f Nothing)
                    Unknown -> do addEdge n e
                                  let cont = do removeEdge n e
                                                runB env n (B m)
                                  insertPQ n cont (queue env)
            IO m -> m >>= go . f
            Fix g -> do r <- newIORef undefined
                        let res = unsafePerformIO (readIORef r)
                        let cont = do x <- g res
                                      op $ IO $ writeIORef r x
                                      f x
                        go cont

data Em a where
  Await  :: [Ex E] -> B (Maybe a, Em a) -> Em a

type Emits a = B (Em a)


data EmitState a = Emit !a | Skip | Unknown

data EState a = EState {
  action     :: !(B ()),
  curEmit    :: !(EmitState a),
  behav      :: ![Weak (IORef a)]
 }

type Nd a = Node (EState a)

data E a = E Env (IO (Nd a))



unsafeLiftIO :: IO a -> B a
unsafeLiftIO = B . op . IO

isNow :: E a -> B (Maybe a)
isNow = B  . op . IsNow

readE :: Nd a -> IO (EmitState a)
readE m = curEmit <$> readNode m

runEmits :: Env -> Emits a -> B (E a)
runEmits env m = do x <- unsafeLiftIO (runEmitsIO env m)
                    return (E env (return x))

runEmitsUnsafe :: Env -> Emits a -> E a
runEmitsUnsafe env e = E env (once (runEmitsIO env e))

runEmitsIO :: Env -> Emits a -> IO (Nd a)
runEmitsIO env e = 
  do n <- newNode (EState (return ()) Unknown [])
     let cont = do Await l m <- e
                   now <- any id <$> mapM (\(Ex  e) -> isJust <$> isNow e) l
                   if now
                   then do x <- m 
                           unsafeLiftIO $ setNode n x
                   else unsafeLiftIO $ setNode n (Nothing, Await l m)
     runB env n cont
     return n where
  setNode n (v, Await l m) =
     do setEmit n (toEmit v) 
        addClean env n
        addEdges n l
        let cont = do unsafeLiftIO $ setEmit n Unknown
                      unsafeLiftIO $ removeEdges n l
                      x <- m 
                      unsafeLiftIO (setNode n x)
        setAction n cont
  addEdges    n l = mapM_ (\(Ex (E _ m)) -> m >>= addEdge    n) l
  removeEdges n l = mapM_ (\(Ex (E _ m)) -> m >>= removeEdge n) l
  toEmit Nothing  = Skip
  toEmit (Just x) = Emit x

addClean :: Env -> Node (EState a) -> IO ()
addClean env n = modifyIORef (envState env) (\x -> x {clean = Ex (Ndd n) : clean x })

setEmit :: Node (EState a) -> EmitState a -> IO ()
setEmit n a = 
  do s <- readNode n
     writeNode n (s {curEmit = a}) 

setAction :: Node (EState a)-> B () -> IO ()
setAction n a = 
  do s <- readNode n
     writeNode n (s {action = a}) 

registerStep :: E a -> IORef a -> IO ()
registerStep (E _ m) r = 
   do n <- m
      s <- readNode n
      wr <- mkWeakIORef r (return ())
      writeNode n (s {behav = wr : behav s}) 


stepb :: a -> E a -> B (B a)
stepb i e = 
  do r <- unsafeLiftIO $ newIORef i
     unsafeLiftIO $ registerStep e r
     return $ unsafeLiftIO (readIORef r)
     

cleanUp :: Env -> IO ()
cleanUp e = 
    do s <- readIORef (envState e)
       mapM_ cleanE (clean s)
       writeIORef (envState e) (s {clean = []}) where

 cleanE (Ex (Ndd n))  =
   do es <- readNode n    
      writeNode n (es {curEmit = Unknown})              
      case curEmit es of
       Emit x -> setRefs x es
       _      -> return () 
  where setRefs x es = 
         do bm <- mapM deRefWeak (behav es)
            mapM (\b -> writeIORef b x) (catMaybes bm)
            let b' = catMaybes $ zipWith (\x y -> x <$ y) (behav es) bm
            writeNode n (es { behav = b', curEmit = Unknown})

handleQueue :: Env -> IO ()
handleQueue env = 
  do h <- dequeue (queue env)
     case h of
       Just m -> m 
       Nothing -> return ()

iteration :: Env -> IO ()
iteration env = do handleQueue env
                   cleanUp env


