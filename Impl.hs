{-# Language ScopedTypeVariables, GADTs,GeneralizedNewtypeDeriving, ViewPatterns, TupleSections #-} 

module Impl(B,E,M, step, valueB, isNow, unionWith, never, filterJust, observeE, switchE, scanE, interpret)  where

import IncTopoSort
import System.Mem.Weak
import Data.IORef
import Data.Unique
import Control.Monad.Fix
import Data.Maybe
import System.IO.Unsafe
import Control.Monad.Reader

-- todo: more same context checks
-- joinB
-- clean up


data EnvState = ES {
  clean    :: ![ExNode EState],
  seqs     :: ![()] -- secretly IO actions
 }

data Env = Env {
  uid      :: !Unique,
  queue    :: !(PrioQueue EState),
  envState :: !(IORef EnvState) 
 }

instance Eq Env where
  a == b = uid a == uid b

data B a = Const a
         | B (IORef a) (E a) -- forcing these values tuple leads to IO actions

never =Never

data E a = Never
         | E !Env (Node (EState a)) -- forcing the node leads to IO actions

{- The B and E types have secret IO actions hidden in them. 
   These IO actions do the following: 
   For events:
     create new nodes in the network, add edges to the network and schedule nodes
   For behavior:  
     * what events can do  
     * create and read and initialize IORefs based on other IORefs 
       of behaviors. These IORefs change during the cleanUp phase and 
       at no other time.

   The IO actions can only be executed on the FRP thread, because
   the datatypes are abstract and hence there is no way of forcing the
   values which lead to IO actions.

   Since the IORefs of behaviors change during cleanup, no "secret" IO actions are 
   executed during this time. (this can be seen because cleanup does not deal with
   the datatypes B and E).

-}


data EState a = EState {
  action     :: !(IO ()),
  curEmit    :: !(Maybe a),
  behav      :: ![Weak (IORef a)]
 }



data Mf a where
  LiftIO  :: IO a -> Mf a
  Step    :: a -> E a -> Mf (B a)
  ValueB  :: B a -> Mf a
  IsNow   :: E a -> Mf (Maybe a)
  Fix    :: (a -> M a) -> Mf a

data M a where
  M      :: Mf a -> M a
  Bind   :: M a -> (a -> M b) -> M b
  Return :: a -> M a

data ViewM a where
  Pure :: a -> ViewM a
  Act  :: Mf a -> (a -> M b) -> ViewM b

instance Monad M where
  return = Return
  (>>=)  = Bind

instance MonadFix M where
  mfix f = M $ Fix f

viewM :: forall a. M a -> ViewM a
viewM (Bind (Bind m f) g) = viewM $ Bind m (\x -> Bind (f x) g) 
viewM (Bind (Return x) f) = viewM $ f x
viewM (Bind (M x) f)      = Act x f
viewM (M m)               = Act m Return 
viewM (Return x)          = Pure x


step i b = M $ Step i b
valueB b = M $ ValueB b
isNow e = M $ IsNow e
unsafeMomentIO i = M $ LiftIO i


-- at any time IO actions

newE :: IO () -> IO (Node (EState a))
newE a = newNode (EState a Nothing [])

dependOn :: Env -> Node (EState a) -> Node (EState b) -> IO ()
dependOn env n m = 
   do addEdge n m
      x <- readE m
      case x of
        Just _ -> insertNodeNub (queue env) (ExN n)
        Nothing -> return ()

registerStep :: IORef a -> E a -> IO ()
registerStep r (E _ n) = do es <- readNode n
                            wr <- mkWeakIORef r (return ())
                            writeNode n (es {behav = wr : behav es})

-- scheduled IO actions

readE :: Node (EState a) -> IO (Maybe a)
readE m = curEmit <$> readNode m

readESure :: Node (EState a) -> IO  a
readESure m = fromJust . curEmit <$> readNode m

readB :: Env -> B a -> IO a
readB e b = do s <- readIORef (envState e)
               writeIORef (envState e) (s {seqs = (res `seq` ()) : seqs s })
               return res
  where res = case b of
               Const x -> x
               B r _ -> unsafePerformIO $ readIORef r


setAction :: Node (EState a) -> IO () -> IO ()
setAction n a = do es <- readNode n
                   writeNode n (es {action = a})


emit :: Env -> a -> Node (EState a)-> IO ()
emit env v n = 
  do es <- readNode n
     writeNode n (es {curEmit = Just v})
     s <- readIORef (envState env)
     scheduleParents (queue env) n
     writeIORef (envState env) (s {clean = ExN n : clean s })
              

unionWith :: (a -> a -> a) -> E a -> E a -> E a
unionWith f Never r = r
unionWith f l Never = l
unionWith f (E el nl) (E er nr) 
  | el /= er = error "Events not from same FRP context!"
  | otherwise = E el n
    where
     n = unsafePerformIO $ 
          do n <- newE combineEm
             dependOn el n nl
             dependOn el n nr
             return n

     combineEm = do l <- readE nl
                    r <- readE nr
                    emit el (combineMaybe l r) n
                  
     combineMaybe (Just x) (Just y) = f x y
     combineMaybe (Just x) Nothing  = x
     combineMaybe Nothing  (Just y) = y

instance Functor E where
  fmap f Never  = Never
  fmap f (E e n) = E e node where
    node =  unsafePerformIO $ 
          do  m <- newE fm
              dependOn e m n
              return m
    fm = do x <- readESure n 
            emit e (f x) node
           
filterJust :: E (Maybe a) -> E a
filterJust Never = Never
filterJust (E e n) = E e node where
  node =  unsafePerformIO $ 
         do  m <- newE fil
             dependOn e m n
             return m
  fil = do x <- readESure n
           case x of
             Just v -> emit e v node
             Nothing -> return ()

switchE :: B (E a) -> E a
switchE (Const x)  = x
switchE (B r (E env n)) = E env node where
  node = unsafePerformIO $ 
         do  e <- readIORef r 
             m <- newE (switchEm e) 
             case e of
               Never -> return ()
               E _ i -> dependOn env m i
             dependOn env m n
             return m
  switchEm i = 
    do x <- readE n
       case x of
        Just Never   -> remove >> setAction node (switched Never) 
        Just e@(E _ v) -> do remove
                             setAction node (switched e) 
                             dependOn env node v
        Nothing -> case i of
                     Never -> return ()
                     E _ i -> do x <- readE i
                                 case x of
                                   Just v -> emit env v node
                                   Nothing -> return ()
     where remove =  case i of
                         Never -> return () 
                         E _ i -> removeEdge node i
  switched e@(E _ i) = 
     do x <- readE i
        case x of
          Just v -> emit env v node
          Nothing -> return ()
        setAction node (switchEm e) 


instance Functor B where
  fmap f (Const x) = Const (f x)
  fmap f (B r e) = B ref evs where
    evs = fmap f e
    ref = unsafePerformIO $
        do v <- readIORef r
           res <- newIORef (f v)
           registerStep res evs
           return res



instance Applicative B where
  pure x = Const x
  (Const f) <*> x = fmap f x
  f <*> (Const x) = fmap ($ x) f
  (B fr (E env fn)) <*> (B vr (E _ vn)) = B ref up where
   ref = unsafePerformIO $
     do f <- readIORef fr
        v <- readIORef vr
        r' <- newIORef (f v)
        registerStep r' up
        return r'
   up = E env node
   node = unsafePerformIO $
        do n <- newE combineEm
           dependOn env n fn
           dependOn env n vn
           return n
          
   combineEm = 
      do l <- readE fn
         r <- readE vn
         v <- case (l,r) of
           (Just x, Just y ) -> return (x y)
           (Just x, Nothing) -> x <$> readIORef vr
           (Nothing, Just y) -> ($ y) <$> readIORef fr
         emit env v node

joinB :: B (B a) -> B a
joinB (Const x) = x
joinB (B r (E env no)) = B ref up where
   ref = unsafePerformIO $
     do b <- readIORef r
        v <- case b of
               Const x -> return x
               B r _   -> readIORef r
        r <- newIORef v
        registerStep r up
        return r
   up = E env node
   node = unsafePerformIO $
    do b <- readIORef r
       n <- newE (switchEm b)
       case b of
           Const x -> return ()
           B _ (E _ ni)   -> dependOn env n ni
       dependOn env n no
       return n

   switchEm b = 
    do x <- readE no
       case x of
        Just (Const x) -> do setAction node (switchEm (Const x))
                             remove
                             emit env x node
        Just b@(B ri' (E _ ni')) -> 
            do remove
               lev <- getLevel ni'
               myLev <- getLevel node
               if myLev > lev
                then do x <- readE ni'
                        v <- case x of
                          Just v  -> return v
                          Nothing -> readIORef ri'
                        emit env v node
                        setAction node (switchEm b)
                else do setAction node (switched b)
                        insertNodeNub (queue env) (ExN node)

        Nothing -> do case b of
                        Const x -> error "weirdness"
                        B r (E _ n) -> 
                          do x <- readE n
                             case x of
                              Just v  -> emit env v node
                              Nothing -> return () 
     where remove =  case b of
                         Const _ -> return () 
                         B _ (E _ i) -> removeEdge node i
   switched b@(B ri (E _ ni)) = 
     do x <- readE ni
        v <- case x of
          Just v -> return v
          Nothing -> readIORef ri
        emit env v node
        setAction node (switchEm b) 

instance Monad B where
  return = pure
  m >>= f = joinB (fmap f m)

stepImp :: Env -> a -> E a -> IO (B a)
stepImp env i e =
  do s <- readIORef (envState env)
     writeIORef (envState env) (s {seqs = (res `seq` ()) : seqs s })
     return res
  where res = case e of
               Never -> Const i
               E _ n  -> unsafePerformIO $
                            do r <- newIORef i
                               registerStep r e
                               return (B r e)

observeE :: E (M a) -> E a
observeE Never   = Never
observeE (E e n) = E e node where
  node = unsafePerformIO $ 
         do  m <- newE interpret
             dependOn e m n
             return m
  interpret = 
    do x <- readESure n
       loop x

  loop (viewM -> Pure x)  = emit e x node
  loop a@(viewM -> Act m f) =
     case m of
       LiftIO m -> m >>= loop . f
       ValueB b -> readB e b >>= loop . f
       Step i e' -> stepImp e i e' >>= loop . f
       IsNow (E e' m) 
        | e /= e' -> error "Not the right FRP context for this moment!"
        | otherwise ->   
            do lev <- getLevel m
               myLev <- getLevel node
               if myLev > lev
               then readE m >>= loop . f
               else do ensureLevel lev node
                       let a' = do x <- a 
                                   unsafeMomentIO (setAction node interpret)
                                   return x
                       setAction node (loop a)
                       insertNodeNub (queue e) (ExN node)
       Fix fx -> 
         do r <- newIORef (error "FRPFix loop")
            let v = unsafePerformIO $ readIORef r
            let a' = do x <- fx v
                        unsafeMomentIO $ writeIORef r x
                        return x
            loop (a' >>= f)

 

doIteration :: Env -> IO ()
doIteration e = visitNodes where
 visitNodes = 
  do x <- dequeue (queue e)
     case x of
      Just (l,(ExN n)) -> 
                    do e <- readNode n
                       action e
                       visitNodes
      Nothing -> doSeqs
 doSeqs = 
   do s <- readIORef (envState e)
      mapM_ (\x -> seq x (return ())) (seqs s)
      b <- isEmptyPqueue (queue e)
      if b 
      then return ()
      else visitNodes
      
cleanUp :: Env -> IO ()
cleanUp e = 
    do s <- readIORef (envState e)
       mapM_ cleanE (clean s)
       writeIORef (envState e) (s {seqs = [], clean = []}) where

 cleanE (ExN n)  =
     do es <- readNode n                  
        let Just x = curEmit es
        bm <- mapM deRefWeak (behav es)
        mapM (\b -> writeIORef b x) (catMaybes bm)
        let b' = catMaybes $ zipWith (\x y -> x <$ y) (behav es) bm
        writeNode n (es { behav = b', curEmit = Nothing})
       
scanE :: (b -> a -> b) -> b -> E a -> M (B b)
scanE f i e = 
  mfix $ \b ->
    let em = (\v -> do i <- valueB b; pure (f i v)) <$> e
    in step i (observeE em)



interpret :: (E a -> M (E b)) -> [Maybe a] -> IO [Maybe b]     
interpret f (h : t) =
  do env <- Env <$> newUnique <*> emptyPqueue <*> newIORef (ES [] []) 
     i <- newE  (return ())
     maybe (return ()) (\x -> emit env x i) h
     fakeE <- newE (return ())
     emit env (f (E env i)) fakeE
     let E _ res = observeE (E env fakeE)
     seq res return () -- construct network.. sneakily
     doIteration env
     e <- readESure res 
     n <- case e of
      Never -> newE (return ())
      E _ n -> return n
     seq n (return ()) -- this forcing might lead to more network
     doIteration env
     loop env i n t where
  loop env i r [] = (\x -> [x]) <$> readE r
  loop env i r (h : t) = 
      do x <- readE r
         cleanUp env
         maybe (return ()) (\x -> emit env x i) h
         doIteration env
         (x :) <$> loop env i r t
     

instance Applicative M where
  pure = return
  (<*>) = ap

instance Functor M where fmap = liftM


        

