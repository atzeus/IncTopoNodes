{-# Language GADTs,GeneralizedNewtypeDeriving, ViewPatterns, TupleSections #-} 

module Impl where

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

data ExF f where
  ExF :: f a -> ExF f

data Ex where
  Ex :: a -> Ex 

data EnvState = ES {
  clean    :: ![ExF E],
  seqs     :: ![Ex],
  curLevel :: !Int,
  curTime  :: !Int
 }

data Env = Env {
  uid      :: !Unique,
  queue    :: !(PrioQueue EState),
  envState :: !(IORef EnvState) 
 }

instance Eq Env where
  a == b = uid a == uid b

data B a = B !(IORef a) !(E a)

data EState a = EState {
  action     :: !(IO ()),
  curEmit    :: !(Maybe a),
  behav      :: ![ Weak (IORef a)]
 }

-- not a newtype, disallow seqing from different thread
data E a = Never
         | E !Env !(Node (EState a))



readE :: Node (EState a) -> IO (Maybe a)
readE m = curEmit <$> readNode m

readESure :: Node (EState a) -> IO  a
readESure m = fromJust . curEmit <$> readNode m

readB :: Env -> B a -> IO a
readB e b = do s <- readIORef (envState e)
               writeIORef (envState e) (s {seqs = Ex res : seqs s })
               return res
  where res = unsafePerformIO $ 
                 let B r _ = b
                 in readIORef r


setAction :: Node (EState a) -> IO () -> IO ()
setAction n a = do es <- readNode n
                   writeNode n (es {action = a})

dependOn :: Env -> Node (EState a) -> Node (EState b) -> IO ()
dependOn e n m = 
   do addEdge n m
      x <- readE m
      case x of
        Just _ -> 
         do s <- readIORef (envState e)
            ml <- getLevel m
            if curLevel s >= ml 
            then insertNodeNub (queue e) (ExN n)
            else return ()
        Nothing -> return ()

emit :: a -> E a -> IO ()
emit v ev@(E e n) = 
  do es <- readNode n
     writeNode n (es {curEmit = Just v})
     s <- readIORef (envState e)
     scheduleParents (queue e) n
     writeIORef (envState e) (s {clean = ExF ev : clean s })
              


      
newE :: IO () -> IO (Node (EState a))
newE a = newNode (EState a Nothing [])


unionWith :: (a -> a -> a) -> E a -> E a -> E a
unionWith f Never r = r
unionWith f l Never = l
unionWith f (E el nl) (E er nr) 
  | el /= er = error "Events not from same FRP context!"
  | otherwise = res
    where
     res =  E el n
     n = unsafePerformIO $ 
          do n <- newE combineEm
             dependOn el n nl
             dependOn el n nr
             return n

     combineEm = do l <- readE nl
                    r <- readE nr
                    emit (combineMaybe l r) res
                  
     combineMaybe (Just x) (Just y) = f x y
     combineMaybe (Just x) Nothing  = x
     combineMaybe Nothing  (Just y) = y

instance Functor E where
  fmap f Never  = Never
  fmap f (E e n) = res where
    res = E e $  unsafePerformIO $ 
          do  m <- newE fm
              dependOn e m n
              return m
    fm = do x <- readESure n 
            emit (f x) res
           
filterJust :: E (Maybe a) -> E a
filterJust Never = Never
filterJust (E e n) = res where
  res = E e $  unsafePerformIO $ 
         do  m <- newE fil
             dependOn e m n
             return m
  fil = do x <- readESure n
           case x of
             Just v -> emit v res
             Nothing -> return ()

switchE :: B (E a) -> E a
switchE (B r Never) = unsafePerformIO $ readIORef r
switchE (B r (E e en)) = res where
  res = E e node
  node = unsafePerformIO $ 
         do  (E _ i) <- readIORef r 
             m <- newE (switchEm i) 
             dependOn e m i
             dependOn e m en
             return m
  switchEm i = 
    do x <- readE en
       case x of
        Just (E _ v) -> do removeEdge node i
                           setAction node (switched v) 
                           dependOn e node v
        Nothing -> do x <- readE i
                      case x of
                         Just v -> emit v res
                         Nothing -> return () 
  switched i = 
     do x <- readE i
        case x of
          Just v -> emit v res
          Nothing -> return ()
        setAction node (switchEm i) 


-- strictness trick
makeB ~(B i e) = B i e

instance Functor B where
  fmap f (B r e) = makeB content where
    content = unsafePerformIO $
        do v <- readIORef r
           makeBehaviorOf (f v) (fmap f e)


instance Applicative B where
  pure x = makeB $ unsafePerformIO $ (`B` Never) <$> newIORef x 
  (B fr fe) <*> (B vr ve) = makeB content where
   content = unsafePerformIO $
        do f <- readIORef fr
           v <- readIORef vr
           r' <- newIORef (f v)
           e <- sampleEm fe ve
           return (B r' e)
          
   sampleEm (E el nl) (E er nr) 
     | el /= er = error "Events not from same FRP context!"
     | otherwise = res where
     res = 
          do n <- newE (return ())
             let res = E el n
             setAction n (combineEm res)
             dependOn el n nl
             dependOn el n nr
             return res

     combineEm node = 
      do l <- readE nl
         r <- readE nr
         v <- case (l,r) of
           (Just x, Just y ) -> return (x y)
           (Just x, Nothing) -> x <$> readIORef vr
           (Nothing, Just y) -> ($ y) <$> readIORef fr
         emit v node


makeBehaviorOf :: a -> E a -> IO (B a)
makeBehaviorOf i ev@(E e m) = 
  do r <- newIORef i
     wr <- mkWeakIORef r (return ())
     es <- readNode m
     writeNode m (es { behav = wr : behav es})
     return (B r ev)



data Mf a where
  LiftIO  :: IO a -> Mf a
  Step    :: a -> E a -> Mf (B a)
  ValueB  :: B a -> Mf a
  IsNow   :: E a -> Mf (Maybe a)

data M a where
  M      :: Mf a -> M a
  MFix   :: (a -> M a) -> M a
  Bind   :: M a -> (a -> M b) -> M b
  Return :: a -> M a

data ViewM a where
  Pure :: a -> ViewM a
  Act  :: Mf a -> (a -> M b) -> ViewM b
  Fix :: (a -> M a) -> ViewM a

instance Monad M where
  return = Return
  (>>=)  = Bind

instance MonadFix M where
  mfix = MFix


step i b = M $ Step i b
valueB b = M $ ValueB b
isNow e = M $ IsNow e
unsafeMomentIO i = M $ LiftIO i

viewM :: M a -> ViewM a
viewM (Bind (Bind m f) g) = viewM $ Bind m (\x -> Bind (f x) g) 
viewM (Bind (Return x) f) = viewM $ f x
viewM (Bind (M x) f)      = Act x f
viewM (M m)               = Act m Return 
--viewM (Bind (MFix f) g)   = Fix $ \x -> Bind (f x) g
viewM (Return x)          = Pure x
viewM (MFix f)            = Fix f


observeE :: E (M a) -> E a
observeE Never   = Never
observeE (E e n) = res where
  res = E e node 
  node = unsafePerformIO $ 
         do  m <- newE interpret
             dependOn e m n
             return m
  interpret = 
    do x <- readESure n
       loop x

  loop (viewM -> Pure x)  = emit x res
  loop a@(viewM -> Act m f) =
     case m of
       ValueB b -> readB e b >>= loop . f
       Step i ev -> makeBehaviorOf i ev >>= loop . f
       IsNow (E e' m) 
        | e /= e' -> error "Not the right FRP context for this moment!"
        | otherwise ->   
            do es <- readIORef (envState e)
               lev <- getLevel n
               if curLevel es > lev 
               then readE m >>= loop . f
               else do ensureLevel lev node
                       let a' = do x <- a 
                                   unsafeMomentIO (setAction node interpret)
                                   return x
                       setAction node (loop a)
                       insertNodeNub (queue e) (ExN node)
  loop (viewM -> Fix f) = 
     do r <- newIORef undefined
        let v = unsafePerformIO $ readIORef r
        let a' = do x <- f v
                    unsafeMomentIO $ writeIORef r x
                    return x
        loop a'

   

doIteration :: Env -> IO ()
doIteration e = 
  do s <- readIORef (envState e)
     writeIORef (envState e) (s {curTime = curTime s + 1 })
     visitNodes where
 visitNodes = 
  do x <- dequeue (queue e)
     case x of
      Just (l,(ExN n)) -> 
                    do s <- readIORef (envState e)
                       writeIORef (envState e) (s {curLevel = max l (curLevel s)})
                       e <- readNode n
                       action e
                       visitNodes
      Nothing -> cleanUp
 cleanUp = 
    do s <- readIORef (envState e)
       mapM_ (\(Ex x) -> seq x (return ())) (seqs s)
       mapM_ cleanE (clean s)
       writeIORef (envState e) (s {curLevel = 0, seqs = [], clean = []})

 cleanE (ExF (E _ n))  =
     do es <- readNode n                  
        let Just x = curEmit es
        bm <- mapM deRefWeak (behav es)
        mapM (\b -> writeIORef b x) (catMaybes bm)
        let b' = catMaybes $ zipWith (\x y -> x <$ y) (behav es) bm
        writeNode n (es { behav = b', curEmit = Nothing})
        
interpret :: (E a -> M (B b)) -> [Maybe a] -> IO [b]     
interpret f (h : t) =
  do env <- Env <$> newUnique <*> emptyPqueue <*> newIORef (ES [] [] 0 0) 
     i <- E env <$> newE  (return ())
     maybe (return ()) (`emit` i) h
     fakeE <- E env <$> newE (return ())
     emit (f i) fakeE
     let E _ res = observeE fakeE
     seq res return () -- construct network.. sneakily
     doIteration env
     (B r _) <- readESure res
     loop env i r t where
  loop env i r [] = (\x -> [x]) <$> readIORef r
  loop env i r (h : t) = 
      do x <- readIORef r
         maybe (return ()) (`emit` i) h
         doIteration env
         loop env i r t
     


instance Applicative M where
  pure = return
  (<*>) = ap

instance Functor M where fmap = liftM


        

