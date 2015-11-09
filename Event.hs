{-# Language Rank2Types,GADTs #-} 

module Event where

import IncTopoSort 
import Data.IORef
import Control.Monad.State
import Control.Monad.Trans

data RTState = RTState { curLevel :: IORef Level, queue :: PrioQueue EvState, curRound :: IORef Integer } deriving Eq

type Time = Integer

data EvState a = EvState { env :: RTState, syntax :: !(EvSyntax a) }
type Var a = Node (EvState a)
data Event a 
  = Stateful { node :: Var a}
  | Occ Time a
  | Never

data EvSyntax a where
  Prim    :: EvSyntax a
  Ref     :: Event a -> EvSyntax a
  First   :: Event a -> Event a -> EvSyntax a
  Bind    :: Event a -> (a -> Event b) -> EvSyntax b

laterThan :: Time -> Event a -> Event a
laterThan t Never        = Never
laterThan t (Occ t' a)   = Occ (max t t') a
laterThan _ e@(Stateful _) = e

eval :: EvSyntax a -> EvSyntax a
eval (First (Occ t x) (Occ t2 y)) 
        | t <= t2   = Ref $ Occ t x
        | otherwise = Ref $ Occ t2 y
eval (First (Occ t x) _) = Ref $ Occ t x
eval (First _ (Occ t x)) = Ref $ Occ t x
eval (Bind (Occ t x) f)  = Ref $ laterThan t (f x)
eval x                 = x

getVal :: EvSyntax a -> Maybe (Event a)
getVal (Ref x)   = Just x
getVal _         = Nothing

substEv :: Event a -> Var x -> Event x -> Event a
substEv (Stateful v') v e | Just conv <- eqConv v v' = conv e
substEv x  _ _ = x

subst :: EvSyntax a -> Var x -> Event x -> EvSyntax a
subst (Ref     e)   v s = Ref (substEv e v s)
subst (First l r) v s = First (substEv l v s) (substEv r v s)
subst (Bind x f)  v s = Bind  (substEv x v s) f
subst x           _ _ = x

schedule :: ExNode EvState -> IO ()
schedule ex@(ExN n) = 
  do st <- readNode n
     insertNode ex (queue (env st))
     
     
substIn :: Var x -> Event x -> ExNode EvState -> IO ()
substIn v e (ExN n) = 
 do st <- readNode n
    let syn = subst (syntax st) v e
    writeNode n (st { syntax = syn })

substSchedule :: Var x -> Event x -> ExNode EvState -> IO ()
substSchedule v e ex = 
  do substIn v e ex
     schedule ex

updateNode :: ExNode EvState -> IO ()
updateNode (ExN n) = 
  do st <- readNode n
     let syn = eval (syntax st)
     writeNode n (st {syntax = syn})
     case getVal syn of
      Just x -> do pr <- getAliveParents n
                   mapM_ (substSchedule n x) pr
      Nothing -> return ()

isUptodate :: Node (EvState a) -> IO Bool
isUptodate n = 
   do st <- readNode n
      lev <- getLevel n
      curLev <- readIORef (curLevel (env st))
      if lev > curLev
      then return False
      else if lev == curLev
           then isInQueue (ExN n) (queue (env st))
           else return True

walkRedirs :: Event a -> IO (Event a)
walkRedirs Never   = pure Never
walkRedirs e@(Occ _ _) = pure e
walkRedirs ei@(Stateful n) =  
  do st <- readNode n
     case syntax st of
      Ref e -> walkRedirs e
      _     -> return ei


newFirst :: Event a -> Event a -> IO (Event a)
newFirst l r = 
  do l <- walkRedirs l
     r <- walkRedirs r
     case l of
      Occ t x -> case r of
         Occ t' y | t' < t -> pure $ Occ t' y
         _                 -> pure $ Occ t x
      Never -> return r
      Stateful ls ->
       case r of
         Never -> return l
         Occ t x -> 
           do u <- isUptodate ls 
              if u 
              then return (Occ t x)
              else 
               do lss <- readNode ls
                  n <- newNode $ EvState (env lss) (First l r) 
                  loop <- addEdge ls n
                  if loop
                  then error "FRP Loop"
                  else return (Stateful n)
         Stateful rs -> 
           do lss <- readNode ls
              rss <- readNode rs
              if env lss /= env rss 
              then error "Events not from same context!"
              else return ()
              n <- newNode $ EvState (env lss) (First l r)
              loop <- addEdge ls n
              if loop
              then error "FRP Loop"
              else return ()
              addEdge rs n
              return (Stateful n)

newPrimEvent :: RTState -> IO (Event a, a -> IO ())
newPrimEvent env = 
   do n <- newNode (EvState env Prim)
      return (Stateful n, update n) where
 
   update n a = 
     do r <- readIORef (curRound env)
        let round = r + 1
        writeNode n $ EvState env (Ref (Occ round a))
        schedule (ExN n)
          

runRound :: RTState -> IO ()
runRound r = 
      do modifyIORef (curRound r) (+1)
         writeIORef (curLevel r) 0
         loop where
  loop =
   do x <- dequeue (queue r)
      case x of
        Just (lev,p) ->
          do writeIORef (curLevel r) lev
             updateNode p
             loop
        Nothing -> return ()
    
                   
                  
           

