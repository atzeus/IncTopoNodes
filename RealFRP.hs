{-# Language  GeneralizedNewtypeDeriving, GADTs #-} 


module RealFRP(Events, Behavior, Now, BLike(..), never, filterJust, unionWith, observe,plan,switchEv,Step(..), switch, justUp,justDown, callback, runFRP) where

import EventNetwork 
import FList 
import Once
import Control.Monad
import Control.Monad.Reader
import Data.IORef

data Events   a = E Env (IO (ENode a))
                | Never
newtype Behavior a = B { runB :: IO a }  deriving (Functor, Applicative,Monad,MonadFix)
type StrongRefs = IORef [Ex ENode]

data NowEnv = NowEnv { strongRefs :: StrongRefs , eenv :: Env } 

newtype Now      a = N (ReaderT NowEnv IO a ) deriving (Functor, Applicative,Monad,MonadFix, MonadIO)

runNow :: NowEnv -> Now a -> IO a
runNow sr (N m) = runReaderT m sr

class BLike m where
  sample :: Behavior a    -> m a
  stepb  :: a -> Events a -> m (Behavior a)

instance BLike Behavior where
  sample = id
  stepb a (E _ m) = B $
    do n <- m
       tr <- newTRef a n 
       return $ B $ readTRef tr

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

plan :: Events (Now a) -> Now (Events a)
plan Never = pure Never
plan (E env m) = N $ 
    do n <- lift $ m
       nenv <- ask
       if eenv nenv /= env
       then error "Now not from same FRP context!"
       else return ()
       r <- lift $ runEmits env (planE nenv n)
       lift $ modifyIORef' (strongRefs nenv) (Ex r :)
       return (E env (return r))

switchEv :: Events a -> Events (Events a) -> Behavior (Events a)
switchEv i Never = pure i
switchEv Never e@(E env _) = 
  B $ do (x,_) <- newBaseE env
         runB $ switchEv (E env (return x)) e
switchEv (E env im) (E _ sm) = B $
    do i <- im
       s <- sm
       r <-  runEmits env (switchE i s)
       return (E env (return r))


switch :: Step (Events a) -> Events a
switch (Step b e@(E env _)) = E env $ once $
  do i <- runB b
     E _ n <- runB $ switchEv i e
     n

unsafeStep :: Behavior a -> Events a -> Behavior a
unsafeStep b (E env m) = B $ 
  do r <- once $
       do i <- runB b
          e <- m
          newTRef i e
     readTRef r 

unsafeAppStep :: Behavior (a -> b) -> Events (a -> b) -> Behavior a -> Events a -> Events b
unsafeAppStep bf (E env fm) bx (E _ xm) = E env $ once $
  do fe <- fm
     xe <- xm
     runEmits env (appStep bf fe bx xe)

callback :: Now (Events a, a -> IO ())
callback = N $
   do env <- ask
      (n,cb) <- lift $ newBaseE (eenv env) 
      return (E (eenv env) (return n), cb)


runFRP :: Now () -> IO (IO ())
runFRP n = do sr <- newIORef []
              eenv <- newEnv
              let env = NowEnv sr eenv
              runNow env n
              return (iteration eenv)

justUp :: Step (Maybe a) -> Events a
justUp (Step b (E env m)) = 
  E env $ once $
        do n <- m
           runEmits env (justUpE b n) 

justDown :: Step (Maybe a) -> Events a
justDown (Step b (E env m)) = 
  E env $ once $
        do n <- m
           runEmits env (justUpE b n) 

joinStepE ::  Step (Step a) -> Events a
joinStepE (Step b Never) = E env $ once $
   do Step _ e <- runB b
      case e of
         Never -> return $ newBaseE e
joinStepE (Step b (E env em)) = E env $ once $
  do n <- em
     


data Step a = Step {initStep :: Behavior a, restStep :: Events a }

instance Functor Step where
  fmap f (Step b e) = 
    let e' = fmap f e
    in Step (unsafeStep (fmap f b) e') e'

instance Applicative Step where
  pure x = Step (pure x) never
  (Step fb fe) <*> (Step xb xe) = 
    let e = unsafeAppStep fb fe xb xe
    in Step (unsafeStep (fb <*> xb) e) e



      
-- Emits implementations

fmapE :: (x -> y) -> ENode x -> Emits y
fmapE f e = loop where
 loop = Await (e :. X) $ \(Just x :. X) ->
      pure $ Commit (Just (f x)) loop

filterJustE :: ENode (Maybe a) -> Emits a
filterJustE e = loop where
  loop = Await (e :. X) $ \(x :. X) ->
          pure $ Commit (join x) loop

unionWithE :: (a -> a -> a) -> ENode a -> ENode a -> Emits a 
unionWithE f l r = loop where
 loop = Await (l :. r :. X) $ \(lv :. rv :. X) ->
         pure (Commit (Just $ combine lv rv) loop)
 combine (Just x) (Just y) = f x y
 combine Nothing  (Just y) = y
 combine (Just x) Nothing  = x

observeE :: ENode (Behavior a) -> Emits a
observeE e = loop where
 loop = Await (e :. X) $ \(Just xm :. X) ->
         do x <- runB xm
            pure $ Commit (Just x) loop

switchE :: ENode a -> ENode (Events a) -> Emits a 
switchE b e = go b where
 go i = Await (i :. e :. X) $ \(iv :. ev :. X) ->
       case ev of
         Just (E _ m) ->
          do i' <- m
             pure $  WaitMore $ Await (i' :. e :. X) $ \(iv' :. _ :. X) ->
                            pure $ Commit iv' (go i')
         Nothing -> pure $ Commit iv (go i)

joinStepE :: ENode a -> ENode (Step a) -> Emits a 
joinStepE b e = go b where
 go i = Await (i :. e :. X) $ \(iv :. ev :. X) ->
       case ev of
         Just (Step ib (E _ m)) ->
          do i' <- m
             pure $  WaitMore $ Await (i' :. e :. X) $ \(iv' :. _ :. X) ->
                            do v <- case iv' of
                                 Nothing -> runB ib  
                                 Just v -> return v
                               return $ Commit (Just v) (go i')
         Nothing -> pure $ Commit iv (go i)

appStep :: Behavior (a -> b) -> ENode (a -> b) -> Behavior a -> ENode a -> Emits b 
appStep bf ef bx ex = loop where
 loop = Await (ef :. ex :. X) $ \(fv :. xv :. X) ->
     do v <- case (fv,xv) of
            (Just f, Just x) -> pure (f x)
            (Just f, Nothing) -> 
              do x <- runB bx
                 return (f x)
            (Nothing, Just x) ->
              do f <- runB bf
                 return (f x)            
        return $ Commit (Just v) loop 
      
justUpE :: Behavior (Maybe a) -> ENode (Maybe a) -> Emits a
justUpE b e = loop where
  loop = Await (e :. X) $ \(Just x :. X) ->
        do p <- runB b
           return $ Commit (combine p x) loop
  combine Nothing r@(Just _) = r
  cobmine _       _          = Nothing

justDownE :: Behavior (Maybe a) -> ENode (Maybe a) -> Emits a
justDownE b e = loop where
  loop = Await (e :. X) $ \(Just x :. X) ->
        do p <- runB b
           return $ Commit (combine p x) loop
  combine l@(Just _) Nothing = l
  cobmine _       _          = Nothing
          

planE :: NowEnv -> ENode (Now a) -> Emits a
planE strongRefs e = loop where
 loop = Await (e :. X) $ \(Just xm :. X) ->
         do x <- runNow strongRefs xm
            pure $ Commit (Just x) loop
