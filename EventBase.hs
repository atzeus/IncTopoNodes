{-# Language Rank2Types,GADTs #-} 

module EventBase(Event, newTimeline, newPrimEvent, newFirst, newBind, scheduleIO, never, at, RTState, currentRound, runRound)  where

import IncTopoSort 
import Data.IORef
import Control.Monad.State
import Control.Monad.Trans
import Debug.Trace
import qualified Data.IntMap.Strict as IM

data RTState = RTState { queue :: PrioQueue EvState, curRound :: IORef Integer, strongRefs :: IORef (IM.IntMap (ExNode EvState)), unique :: IORef Int } deriving Eq

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
  Bind    :: Event a -> (a -> IO (Event b)) -> EvSyntax b

laterThan :: Time -> Event a -> Event a
laterThan t Never        = Never
laterThan t (Occ t' a)   = Occ (max t t') a
laterThan _ e@(Stateful _) = e

eval :: EvSyntax a -> IO (EvSyntax a)
eval (First (Occ t x) (Occ t2 y)) 
        | t <= t2        = return $ Ref $ Occ t x
        | otherwise      = return $ Ref $ Occ t2 y
eval (First (Occ t x) _) = return $ Ref $ Occ t x
eval (First _ (Occ t x)) = return $ Ref $ Occ t x
eval (Bind (Occ t x) f)  = Ref . laterThan t  <$>  f x
eval x                   = return x

substEv :: Event a -> Var x -> Event x -> Event a
substEv (Stateful v') v e | Just conv <- eqConv v v' = conv e
substEv x  _ _ = x

subst :: EvSyntax a -> Var x -> Event x -> EvSyntax a
subst (Ref     e)   v s = Ref   (substEv e v s)
subst (First l r)   v s = First (substEv l v s) (substEv r v s)
subst (Bind x f)    v s = Bind  (substEv x v s) f
subst x             _ _ = x

schedule :: ExNode EvState -> IO ()
schedule ex@(ExN n) = 
  do st <- readNode n
     insertNode ex (queue (env st))
     
     
substIn :: Var x -> Event x -> ExNode EvState -> IO ()
substIn v e (ExN n) = 
 do st <- readNode n
    let syn = subst (syntax st) v e
    writeNode n (st { syntax = syn })
    case e of 
     Stateful m -> do b <- addEdge n m 
                      if not b 
                      then error "FRP Loop subst"
                      else return ()	
     _          -> return ()

substSchedule :: Var x -> Event x -> ExNode EvState -> IO ()
substSchedule v e ex = 
  do substIn v e ex
     schedule ex

updateNode :: ExNode EvState -> IO ()
updateNode (ExN n) = 
  do 
     st <- readNode n
     syn <- eval (syntax st)
     writeNode n (st {syntax = syn})
     case syn of
      Ref x    -> do pr <- getAliveParents n
                     removeParents n 
                     mapM_ (substSchedule n x) pr
                     
      _  -> return ()

walkRedirs :: Event a -> IO (Event a)
walkRedirs Never       = pure Never
walkRedirs e@(Occ _ _) = pure e
walkRedirs ei@(Stateful n) =  
  do st <- readNode n
     
     case syntax st of
      Ref e -> do -- writeNode n (st{ syntax = error "Busy!"})
                  e' <- walkRedirs e
                  --writeNode n (st{ syntax = Ref e'})
                  return e'
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
           do lss <- readNode ls
              n <- newNode $ EvState (env lss) (First l r) 
              loop <- addEdge ls n
              if not loop
              then error "FRP Loop"
              else do schedule (ExN n)
                      return (Stateful n)
         Stateful rs -> 
           do lss <- readNode ls
              rss <- readNode rs
              if env lss /= env rss 
              then error "Events not from same context!"
              else return ()
              n <- newNode $ EvState (env lss) (First l r)
              loop <- addEdge n ls
              if not loop
              then error "FRP Loop"
              else return ()
              addEdge n rs
              return (Stateful n)

newBind :: Event a -> (a -> IO (Event b)) -> IO (Event b)
newBind e f = 
  do e <- walkRedirs e
     case e of
      Occ t x -> laterThan t <$> f x
      Never -> pure Never
      Stateful es -> 
       do ess <- readNode es
          n <- newNode $ EvState (env ess) (Bind e f)
          loop <- addEdge n es
          if not loop 
          then error "FRP Loop"
          else return ()
          return (Stateful n)
     

runRound :: RTState -> IO ()
runRound r = loop where
  loop =
   do x <- dequeue (queue r)

      case x of
        Just (lev,p) ->
          do updateNode p
             loop
        Nothing -> do modifyIORef (curRound r) (+ 1)
                      return ()

newPrimEvent :: RTState -> IO (Event a, a -> IO ())
newPrimEvent env = 
   do n <- newNode (EvState env Prim)
      return (Stateful n, update n) where
 
   update n a = 
     do r <- readIORef (curRound env)
        let round = r + 1
        writeNode n $ EvState env (Ref (Occ round a))
        schedule (ExN n)
          

removeNr :: Int -> RTState -> IO ()
removeNr i s = 
     do sr <- readIORef (strongRefs s)
        writeIORef (strongRefs s) (IM.delete i sr)

scheduleIO :: Event (IO a) -> IO (Event a)
scheduleIO Never     = return Never
scheduleIO (Occ t x) = Occ t <$> x
scheduleIO (Stateful es) = 
    do ess <- readNode es
       let ev = env ess
       nr <- readIORef (unique ev) 
       writeIORef (unique ev) (nr + 1)
       res@(Stateful e') <- newBind (Stateful es) (\x -> do a <- x; removeNr nr ev; return (Occ 0 a))
       modifyIORef (strongRefs ev) (IM.insert nr (ExN e')) 
       return res
    

currentRound :: RTState -> IO Integer
currentRound s = readIORef (curRound s)

newTimeline :: IO RTState                   
newTimeline =
  RTState <$>  emptyPqueue <*> newIORef 0 <*> newIORef IM.empty <*> newIORef 0
     
                  
           
never = Never

at :: Integer -> a -> Event a
at = Occ 

