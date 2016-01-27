{-# Language FunctionalDependencies,TypeSynonymInstances, MultiParamTypeClasses,ScopedTypeVariables, GADTs,GeneralizedNewtypeDeriving, ViewPatterns, TupleSections #-} 

import Control.Monad.Writer
import Control.Monad.State
import Data.List
import Data.Maybe

data Ex f  where
  Ex :: f a -> Ex f


data Em a where
  Await  :: [Ex E] -> B (Maybe a, Em a) -> Em a

type Emits a = B (Em a)

nevere :: Emits a
nevere = pure (Await [] undefined) 

fmape :: (x -> y) -> E x -> Emits y
fmape f e = pure loop where
 loop = Await [Ex e] $ 
    do Just x <- isNow e
       return (Just (f x), loop)

unionWith :: (a -> a -> a) -> E a -> E a -> Emits a 
unionWith f l r = pure loop where
 loop = Await [Ex l, Ex r] $
   do v <- combine <$> isNow l <*> isNow r
      return (Just v, loop)
 combine (Just x) (Just y) = f x y
 combine Nothing  (Just y) = y
 combine (Just x) Nothing  = x


filterJust :: E (Maybe a) -> Emits a
filterJust e = pure loop where
  loop = Await [Ex e] $
          do v <- isNow e
             return (join v, loop)

              
observeE :: E (B a) -> Emits a
observeE e =  pure loop where
 loop = Await [Ex e] $ 
    do Just x <- isNow e
       v <- x
       return (Just v, loop)


switchBE :: B (E a) -> E (E a) -> Emits a 
switchBE b e = 
  do i <- b
     return (go i) where
 go i = Await [Ex i, Ex e] $ 
    do em <- isNow e
       case em of
         Just i' -> do s <- isNow i'   
                       return (s, go i')
         Nothing -> do im <- isNow i
                       return (im, go i)

type Time = Int

newtype B a = B {runB :: Time -> a } deriving (Functor,Applicative, Monad,MonadFix)
newtype E a = E {getE :: [Maybe a] }


isNow :: E a -> B (Maybe a)
isNow (E e) = B $ \t -> e !! t

stepb :: a -> E a -> B (B a)
stepb i (E l) = B $ \t -> B $ \t' -> (scanl fromMaybe i (drop t l)) !! (t' - t)

-- proof obligation : b == const x
unsafeGetBe :: B (E a) -> E a
unsafeGetBe b = runB b 0

runEm :: Emits a -> B (E a)
runEm e =  B $ \t -> 
        E $ replicate t Nothing ++ loop (runB e t) t where
  
 loop (Await l m) t = 
     let w       = getWait l t
         wl      = length w
         (em,tt) = runB m (t + wl)
         rest    = loop tt (t + wl + 1)
     in w ++ (em : rest)
     
     
getWait :: [Ex E] -> Time -> [Maybe a]
getWait [] _ = repeat Nothing
getWait l t  = Nothing <$ takeWhile (all not) (transpose l')
  where l' = map (\(Ex (E l)) -> map isJust l) l


{-

class (MonadFix b) => FRPImpl b e em | b -> e, e -> b, e -> em where
 emit   :: Monad (em x) => x -> em x ()
 await  :: Monad (em x) => [e a]  -> em x [Maybe a]
 liftB  :: Monad (em x) => b a    -> em x a
 isNow  :: e a    -> b (Maybe a)
 step   :: a -> e a -> b (b a)
 runEm  :: em a x   -> e a
 unsafeStep :: b a -> e a -> e a
 


instance FRPImpl B E EmitM where
-}

