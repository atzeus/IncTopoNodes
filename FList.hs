{-# Language RankNTypes,TypeOperators,DataKinds,FlexibleInstances,ScopedTypeVariables, GADTs,GeneralizedNewtypeDeriving, ViewPatterns, TupleSections #-} 

module FList where

data FList f l where
  X :: FList f '[]
  (:.) :: f h -> FList f t -> FList f (h ': t)

infixr 5 :.

mapfM :: Applicative m => (forall x. f x -> m (g x)) -> FList f l -> m (FList g l)
mapfM f X        = pure X
mapfM f (h :. t) = (:.) <$> f h <*> mapfM f t

mapfM_ :: Applicative m => (forall x. f x -> m ()) -> FList f l -> m ()
mapfM_ f X        = pure ()
mapfM_ f (h :. t) = f h *> mapfM_ f t


mapf :: (forall x. f x -> g x) -> FList f l -> FList g l
mapf f X     = X
mapf f (h :. t) = f h :. mapf f t

toList :: (forall x. f x -> a) -> FList f l -> [a]
toList f X     = []
toList f (h :. t) = f h : toList f t


