{-# Language RankNTypes,TypeOperators, DataKinds,FunctionalDependencies,TypeSynonymInstances, MultiParamTypeClasses,ScopedTypeVariables, GADTs,GeneralizedNewtypeDeriving, ViewPatterns, TupleSections #-}
module Emits (never, filterJust, unionWith, observeE, Behavior(..), prev, updates,combBN, sampleS, joinB, switchE, scanlEM ,justUp, justDown, scanlE, stepB,atn) where
import RealEnv
import Control.Monad
import Ex
import Data.Maybe
import Control.Monad.Fix
import Debug.Trace

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
 combine Nothing Nothing   = error "Cannot happen"

observeE :: Events (Now a) -> Events a
observeE e = unsafeRunEm loop where
 loop = Await [Ex e] $
      do Just v <- isNow e
         x <- v
         pure $ Commit (Just x) loop

data Behavior a = Step (Pb a,Events a)

prev' :: Behavior a -> Pb a
prev' (Step (b,_)) = b

prev :: Behavior a -> Now a
prev s = liftPb (prev' s)

updates :: Behavior a -> Events a
updates (Step (_,e)) = e


unsafeStep :: Pb a -> Emits a -> Behavior a
unsafeStep b a = unsafeRunPb $
  do e <- runEm a
     i <- b
     b' <- hold i e
     return (Step (b',e))

instance Functor Behavior where
  fmap f (Step (b,e)) = unsafeStep (fmap f b) (fmapem f e)

sampleS :: Behavior a -> Now a
sampleS (Step (b,e)) =
   do x <- isNow e
      case x of
        Just v  -> return v
        Nothing -> liftPb b

instance Applicative Behavior where
  pure x = Step (pure x, never)
  fs@(Step (fb,fe)) <*> xs@(Step (xb,xe)) =
      unsafeStep (fb <*> xb) awaits where
   awaits = Await [Ex fe, Ex xe] $
      do f <- sampleS fs
         x <- sampleS xs
         pure $ Commit (Just (f x)) awaits



joinB :: Behavior (Behavior a) -> Behavior a
joinB (Step (b,s)) = unsafeRunPb $
  do si@(Step (ib,_)) <- b
     e <- runEm (joins si)
     i <- ib
     b' <- hold i e
     return (Step (b',e)) where
  joins i@(Step (ib,ie)) = Await [Ex ie, Ex s] $
       do si <- isNow s
          let i' = fromMaybe i si
          x <- sampleS i'
          pure $ Commit (Just x) (joins i')

instance Monad Behavior where
  return x = Step (pure x, never)
  m >>= f = joinB (fmap f m)

switchE :: Behavior (Events a) -> Events a
switchE (Step (b,s)) = unsafeRunPb $
   do ei <- b
      runEm (switch ei)
 where
  switch i = Await [Ex i, Ex s] $
       do si <- isNow s
          let i' = fromMaybe i si
          m <- isNow i'
          pure $ Commit m (switch i')

justUp :: Behavior (Maybe a) -> Events a
justUp (Step (b,s)) = unsafeRunEm loop
 where
  loop = Await [Ex s] $
       do Just c <- isNow s
          p <- liftPb b
          pure $ Commit (combine p c) loop
  combine Nothing r@(Just _) = r
  combine _       _          = Nothing

justDown :: Behavior (Maybe a) -> Events a
justDown (Step (b,s)) = unsafeRunEm  loop
 where
  loop = Await [Ex s] $
       do Just c <- isNow s
          p <- liftPb b
          pure $ Commit (combine p c) loop
  combine l@(Just _) Nothing = l
  combine _       _          = Nothing


stepB :: a -> Events a -> Now (Behavior a)
stepB i e = liftPb $
   do b <-  hold i e
      return (Step (b,e))

scanlE :: (b -> a -> b) -> b -> Events a  -> Now (Behavior b)
scanlE f i e = liftPb $ mfix (\s -> do
      r <- hold i (updates s)
      e <- runEmLazy (loop r)
      return (Step (r,e))) where
  loop r = Await [Ex e] $
       do Just v <- isNow e
          i <- liftPb r
          pure $ Commit (Just $ f i v) (loop r)

combBN :: Behavior (a -> b) -> Behavior a -> Behavior b
combBN (Step (ib,u)) (Step (n,_)) = unsafeRunPb $ do
     e <- runEm (combem u n)
     return (Step (ib <*> n,e)) where
  combem u n = loop where
    loop = Await [Ex u] $
       do Just f <- isNow u
          x <- liftPb n
          pure $ Commit (Just (f x)) loop

scanlEM :: (b -> a -> Now b) -> b -> Events a -> Now (Behavior b)
scanlEM f i e = liftPb $ mfix (\s -> do
      r <- hold i (updates s)
      e <- runEmLazy (loop r)
      return (Step (r,e))) where
  loop r = Await [Ex e] $
       do Just v <- isNow e
          i <- liftPb r
          i' <- f i v
          pure $ Commit (Just $ i') (loop r)

atn :: Now (x -> y) -> Events x -> Events y
atn b e = unsafeRunEm loop where
 loop = Await [Ex e] $
      do Just x <- isNow e
         f <- b
         pure $ Commit (Just (f x)) loop
