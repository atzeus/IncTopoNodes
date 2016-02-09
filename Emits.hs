{-# Language RankNTypes,TypeOperators, DataKinds,FunctionalDependencies,TypeSynonymInstances, MultiParamTypeClasses,ScopedTypeVariables, GADTs,GeneralizedNewtypeDeriving, ViewPatterns, TupleSections #-} 
module Emits (never, filterJust, unionWith, observeE, Step, prev, updates, sample, joinStep, switchE, justUp, justDown, scanlE) where
import RealEnv
import Control.Monad
import Ex
import Data.Maybe
import Control.Monad.Fix

unsafeRunEm :: Emits a -> Events a
unsafeRunEm e = unsafeRunPb (runEm e)

never :: Events a
never = unsafeRunEm $ Await [] undefined

instance Functor Events where
  fmap f e = unsafeRunEm (fmapem f e) where

fmapem :: (a -> b) -> Events a -> Emits b
fmapem f e = loop where
   loop = Await [Ex e] $
      do v <- isNow e
         pure $ Commit (fmap f v) loop

filterJust :: Events (Maybe a) -> Events a
filterJust e = unsafeRunEm loop where
  loop = Await [Ex e] $
      do v <- isNow e
         pure $ Commit (join v) loop

unionWith :: (a -> a -> a) -> Events a -> Events a -> Events a 
unionWith f l r = unsafeRunEm loop where
 loop = Await [Ex l, Ex r] $
      do lv <- isNow l
         rv <- isNow r
         pure $ Commit (Just $ combine lv rv) loop
 combine (Just x) (Just y) = f x y
 combine Nothing  (Just y) = y
 combine (Just x) Nothing  = x
  
observeE :: Events (Behavior a) -> Events a
observeE e = unsafeRunEm loop where
 loop = Await [Ex e] $
      do Just v <- isNow e
         x <- v
         pure $ Commit (Just x) loop

data Step a = Step (Pb a,Events a)

prev' :: Step a -> Pb a
prev' (Step (b,e)) = b

prev :: Step a -> Behavior a
prev s = liftPb (prev' s)

updates :: Step a -> Events a
updates (Step (b,e)) = e


unsafeStep :: Pb a -> Emits a -> Step a
unsafeStep b a = unsafeRunPb $
  do e <- runEm a
     i <- b
     b' <- hold i e
     return (Step (b',e))

instance Functor Step where
  fmap f (Step (b,e)) = unsafeStep (fmap f b) (fmapem f e)

sample :: Step a -> Behavior a
sample (Step (b,e)) = 
   do x <- isNow e
      case x of
        Just v  -> return v
        Nothing -> liftPb b

instance Applicative Step where
  pure x = Step (pure x, never)
  fs@(Step (fb,fe)) <*> xs@(Step (xb,xe)) = 
      unsafeStep (fb <*> xb) awaits where
   awaits = Await [Ex fe, Ex xe] $
      do f <- sample fs
         x <- sample xs
         pure $ Commit (Just (f x)) awaits



joinStep :: Step (Step a) -> Step a
joinStep (Step (b,s)) = unsafeRunPb $
  do si@(Step (ib,_)) <- b
     e <- runEm (joins si)
     i <- ib
     b' <- hold i e
     return (Step (b',e)) where
  joins i@(Step (ib,ie)) = Await [Ex ie, Ex s] $
       do si <- isNow s
          let i' = fromMaybe i si
          x <- sample i'
          pure $ Commit (Just x) (joins i')

instance Monad Step where
  return x = Step (pure x, never)
  m >>= f = joinStep (fmap f m)

switchE :: Step (Events a) -> Events a 
switchE (Step (b,s)) = unsafeRunPb $
   do ei <- b
      runEm (switch ei)
 where
  switch i = Await [Ex i, Ex s] $
       do si <- isNow s
          let i' = fromMaybe i si
          m <- isNow i' 
          pure $ Commit m (switch i') 

justUp :: Step (Maybe a) -> Events a
justUp (Step (b,s)) = unsafeRunEm loop 
 where
  loop = Await [Ex s] $
       do Just c <- isNow s
          p <- liftPb b
          pure $ Commit (combine p c) loop 
  combine Nothing r@(Just _) = r
  combine _       _          = Nothing

justDown :: Step (Maybe a) -> Events a
justDown (Step (b,s)) = unsafeRunEm  loop 
 where
  loop = Await [Ex s] $
       do Just c <- isNow s
          p <- liftPb b
          pure $ Commit (combine p c) loop 
  combine l@(Just _) Nothing = l
  combine _       _          = Nothing


scanlE :: (b -> a -> b) -> b -> Events a  -> Pb (Step b)
scanlE f i e = mfix (\s -> do
      r <- hold i (updates s)
      e <- runEm (loop r) 
      return (Step (r,e))) where
  loop r = Await [Ex e] $
       do Just v <- isNow e
          i <- liftPb r
          pure $ Commit (Just $ f i v) (loop r)
      

