{-# Language GADTs #-} 

module OpMonad(M, ViewM(..), op,viewM) where

import Control.Monad

data M f a where
  M      :: f a -> M f a
  Bind   :: M f a -> (a -> M f b) -> M f b
  Return :: a -> M f a

data ViewM f a where
  Pure :: a -> ViewM f a
  Act  :: f a -> (a -> M f b) -> ViewM f b

instance Monad (M f) where
  return = Return
  (>>=)  = Bind

instance Applicative (M f) where
  pure = Return
  (<*>) = ap

instance Functor (M f) where
  fmap = liftM

op :: f a -> M f a
op = M

viewM :: M f a -> ViewM f a
viewM (Bind (Bind m f) g) = viewM $ Bind m (\x -> Bind (f x) g) 
viewM (Bind (Return x) f) = viewM $ f x
viewM (Bind (M x) f)      = Act x f
viewM (M m)               = Act m Return 
viewM (Return x)          = Pure x
