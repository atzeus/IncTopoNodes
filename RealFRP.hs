{-# Language  GeneralizedNewtypeDeriving, GADTs #-} 


module RealFRP(Events, Behavior, Now, BLike(..), 
     never, filterJust, unionWith, observe,plan,switchEv,
     Step(..), getPrev, getUpdates, switch, justUp,justDown,scanS, 
     callback, runFRP) where

import EventNetwork 
import Once
import Control.Monad
import Control.Monad.Reader
import Data.IORef
import Data.Maybe 

data Events   a = E Env (ENode a))
                | Never
newtype Behavior a = B { runB :: Obs a }  deriving (Functor, Applicative,Monad,MonadFix)
type StrongRefs = IORef [Ex ENode]

data NowEnv = NowEnv { strongRefs :: StrongRefs , eenv :: Env } 

newtype Now a = N (ReaderT NowEnv Obs a ) deriving (Functor, Applicative,Monad,MonadFix, MonadIO)

runNow :: NowEnv -> Now a -> Obs a
runNow sr (N m) = runReaderT m sr

isNow :: Events a -> Behavior (Maybe a)
isNow (E _ m) = B $
    do n <- liftIO m
       checkNode n

class BLike m where
  sample :: Behavior a    -> m a
  stepb  :: a -> Events a -> m (Behavior a)

instance BLike Behavior where
  sample = id
  stepb a (E _ m) = B $
    do tr <- liftIO $ 
              do n <- m
                 newTRef a n 
       return $ B $ liftIO $ readTRef tr

instance BLike Now where
  sample b = N $ lift $ runB b
  stepb a e = N $ lift $ runB $ stepb a e

never :: Events a
never = Never

instance Functor Events where
  fmap f Never = Never
  fmap f (E env m) = 
    E env $ once $
       do n <- m
          runEmits env (fmapE f n) 

filterJust :: Events (Maybe a) -> Events a
filterJust Never = Never
filterJust (E env m) = 
      E env $ once $
        do n <- m
           runEmits env (filterJustE n) 


unionWith :: (a -> a -> a) -> Events a -> Events a -> Events a 
unionWith f l Never = l
unionWith f Never r = r
unionWith f (E env ml) (E _ mr) = 
  E env $ once $ 
    do l <- ml
       r <- mr
       runEmits env (unionWithE f l r) 

observe :: Events (Behavior a) -> Events a
observe Never = Never 
observe (E env m) = 
  E env $ once $ 
    do n <- m
       runEmits env (observeE n) 

getE :: Events a -> IO (Maybe (ENode a))
getE Never = pure Nothing
getE (E _ m) = Just <$> m

switchEv :: Events a -> Events (Events a) -> Behavior (Events a)
switchEv i Never = pure i
switchEv e (E env sm) = B $
    do r <- liftIO $ 
             do i <- getE e 
                s <- sm
                runEmits env (switchE i s) 
       return (E env (return r))


plan :: Events (Now a) -> Now (Events a)
plan Never = pure Never
plan (E env m) = N $ 
    do n <- liftIO m
       nenv <- ask
       if eenv nenv /= env
       then error "Now not from same FRP context!"
       else return ()
       r <- liftIO $ runEmits env (planE nenv n)
       liftIO $ modifyIORef' (strongRefs nenv) (Ex r :)
       return (E env (return r))

callback :: Now (Events a, a -> IO ())
callback = N $
   ask >>= \env ->
     liftIO $ 
      do (n,cb) <- newBaseE (eenv env) 
         return (E (eenv env) (return n), cb)


data Step a = Const a
            | Step Env (IO (TRef a, ENode a)) 

getPrev :: Step a -> Behavior a
getPrev (Const x) = pure x
getPrev (Step _ m) = B $ liftIO $ m >>= readTRef .fst 

getUpdates :: Step a -> Events a
getUpdates (Const _) = never
getUpdates (Step env m) = E env $ snd <$> m

instance Functor Step where
  fmap f (Const x) = Const (f x)
  fmap f (Step env m) = Step env $ once $
           do (t,n) <- m
              i <- readTRef t
              n' <- runEmits env (fmapE f n) 
              t' <- newTRef (f i) n'
              return (t',n') 
              

instance Applicative Step where
  pure x = Const x
  (Const f) <*> (Const x) = Const (f x)
  (Const f) <*> s = fmap f s
  s <*> (Const x) = fmap ($ x) s
  (Step env fm) <*> (Step _ xm) = Step env $ once $
    do (tf,nf) <- fm
       (tx,nx) <- xm
       fi <- readTRef tf 
       xi <- readTRef tx
       n <- runEmits env (appStep tf nf tx nx) 
       t <- newTRef (fi xi) n
       return (t,n)

joinStep :: Step (Step a) -> Step a
joinStep (Const x) = x
joinStep (Step env m) = Step env $ once $ 
  do (t,n) <- m
     i <- readTRef t 
     (iv,ie) <- case i of
       Const x -> pure (x, Nothing)
       Step _ mi -> do (ti,ie) <- mi
                       i <- readTRef ti
                       return (i, Just ie)
     n' <- runEmits env (joinStepE ie n) 
     t' <- newTRef iv n' 
     return (t',n')
        

switch :: Step (Events a) -> Events a
switch (Const x) = x
switch (Step env m) = E env $ once $
  do (t,n) <- m
     i <- readTRef t
     im <- getE i
     runEmits env $ switchE im n


justUp :: Step (Maybe a) -> Events a
justUp (Const x) = never
justUp (Step env m) = E env $ once $
        do (t,n) <- m
           runEmits env (justUpE t n) 

justDown :: Step (Maybe a) -> Events a
justDown (Const x) = never
justDown  (Step env m) = E env $  once $
        do (t,n) <- m
           runEmits env (justDownE t n) 

scanS :: (b -> a -> b) -> b -> Events a -> Step b
scanS f i Never = Const i
scanS f i (E env m) = Step env $ once $ 
  do n <- m
     mfix $ \p -> 
      do n' <- runEmits env (scanlE f (fst p) n)  
         t <- newTRef i n'
         return (t,n')

instance Monad Step where
  return = pure
  m >>= f = joinStep (fmap f m)


runFRP :: Now () -> IO (IO ())
runFRP n = do sr <- newIORef []
              eenv <- newEnv
              let env = NowEnv sr eenv
              runObsMDirect eenv $ runNow env n
              return (iteration eenv)




-- Emits implementations

fmapE :: (x -> y) -> ENode x -> Emits y
fmapE f e = loop where
 loop = Await [Ex e] $
      do v <- checkNode e
         pure $ Commit (fmap f v) loop

filterJustE :: ENode (Maybe a) -> Emits a
filterJustE e = loop where
  loop = Await [Ex e] $
      do v <- checkNode e
         pure $ Commit (join v) loop

unionWithE :: (a -> a -> a) -> ENode a -> ENode a -> Emits a 
unionWithE f l r = loop where
 loop = Await [Ex l, Ex r] $
      do lv <- checkNode l
         rv <- checkNode r
         pure $ Commit (Just $ combine lv rv) loop
 combine (Just x) (Just y) = f x y
 combine Nothing  (Just y) = y
 combine (Just x) Nothing  = x

observeE :: ENode (Behavior a) -> Emits a
observeE e = loop where
 loop = Await [Ex e] $
      do Just v <- checkNode e
         x <- runB v
         pure $ Commit (Just x) loop




switchE :: Maybe (ENode a) -> ENode (Events a) -> Emits a 
switchE b e = loop b where
 loop Nothing = Await [Ex e] $
                do Just i <- checkNode e
                   handleNew i
 loop (Just i) = Await [Ex e, Ex i] $
     do ev <- checkNode e
        case ev of
          Just e -> handleNew e
          Nothing -> do c <- checkNode i
                        pure $ Commit c (loop (Just i))
 handleNew i = case i of
                Never -> pure $ Commit Nothing (loop Nothing)
                E _ m -> do n <- liftIO m
                            v <- checkNode n
                            pure $ Commit v (loop (Just n))


joinStepE :: Maybe (ENode a) -> ENode (Step a) -> Emits a 
joinStepE b e = loop b where
 loop Nothing = Await [Ex e] $
                do Just i <- checkNode e
                   handleNew i
 loop (Just i) = Await [Ex e, Ex i] $
     do ev <- checkNode e
        case ev of
          Just e -> handleNew e
          Nothing -> do c <- checkNode i
                        pure $ Commit c (loop (Just i))
 handleNew i = case i of
                Const x -> pure $ Commit (Just x) (loop Nothing)
                Step _ m ->
                    do (t,n) <- liftIO m
                       v <- checkNode n
                       i <- case v of
                           Just x -> pure x
                           Nothing -> liftIO $ readTRef t
                       pure $ Commit (Just i) (loop (Just n))


planE :: NowEnv -> ENode (Now a) -> Emits a
planE strongRefs e = loop where
 loop = Await [Ex e] $
         do Just xm <- checkNode e
            x <- runNow strongRefs xm
            pure $ Commit (Just x) loop        


appStep :: TRef (a -> b) -> ENode (a -> b) -> TRef a -> ENode a -> Emits b 
appStep bf ef bx ex = loop where
 loop = Await [Ex ef, Ex ex] $ 
     do fm <- maybeGet bf ef
        xm <- maybeGet bx ex
        return $ Commit (Just (fm xm)) loop
  where maybeGet bx ex =
          do mx <- checkNode ex
             case mx of
               Just x  -> pure x 
               Nothing -> liftIO $ readTRef bx

justUpE :: TRef (Maybe a) -> ENode (Maybe a) -> Emits a
justUpE b e = loop where
  loop = Await [Ex e] $
        do p <- liftIO $ readTRef b 
           Just x <- checkNode e 
           return $ Commit (combine p x) loop
  combine Nothing r@(Just _) = r
  combine _       _          = Nothing

justDownE :: TRef (Maybe a) -> ENode (Maybe a) -> Emits a
justDownE b e = loop where
  loop = Await [Ex e] $
        do p <- liftIO $ readTRef b 
           Just x <- checkNode e 
           return $ Commit (combine p x) loop
  combine l@(Just _) Nothing = l
  combine _       _          = Nothing

scanlE :: (b -> a -> b) -> TRef b -> ENode a  -> Emits b
scanlE f b e = loop where
  loop = Await [Ex e] $
        do p <- liftIO $ readTRef b 
           Just x <- checkNode e 
           return $ Commit (Just (f p x)) loop
          

