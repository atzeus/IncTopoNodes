{-# LANGUAGE GeneralizedNewtypeDeriving,DeriveFunctor,StandaloneDeriving, ViewPatterns, TupleSections #-}
module Sem where 
import Control.Monad
import Control.Monad.Fix
import Test.QuickCheck hiding (sample)
import Data.Maybe

-- not part of interface
type Time = Int
-- 

-- types of interface 
newtype B  a = B (Time -> a) deriving (Functor, Applicative, Monad, MonadFix)
data E a = E {getE :: [(Time,a)]} deriving Functor -- monotonically increasing times

-- data Step a = Step { curVal :: B a , updates :: E a }

forgetE :: E a -> Time -> [(Time,a)]
forgetE (E l) t =  dropWhile ((< t) . fst) l

indexAt :: a -> [(Time,a)] -> Time -> a
indexAt i ((et,v) : te) t
    | et < t   = indexAt v te t
indexAt i _ _ = i


-- interface, types B,E,M abstract!

never :: E a
never = E []

unionWith :: (a -> a -> a) -> E a -> E a -> E a
unionWith f (E l) (E r) = E $ go l r where
  go l r = 
   case (l,r) of
     ([],_) -> r
     (_, []) -> l
     ((lt,lv):tl ,(rt,rv):tr)
       | lt < rt  -> (lt, lv)      : go tl r
       | lt == rt -> (lt, f lv rv) : go tl tr
       | lt > rt  -> (rt, rv)      : go l  tr
       

filterJust :: E (Maybe a) -> E a
filterJust (E l) = E $ catMaybes $ map (\(t,m) -> fmap (t,) m) l

isNow :: E a -> B (Maybe a)
isNow e = B $ \t -> case forgetE e t of
                   (t',v) : _ | t == t' -> Just v
                   _                    -> Nothing

observeE :: E (B a) -> E a 
observeE (E l) = E $ map (\(t,B f) -> (t, f t)) l

stepb :: a -> E a -> B (B a)
stepb i e = B $ \t -> B $ indexAt i (forgetE e t)


-- proof obligation to use this function:
-- switchEb b e
-- assure that: hasUpdates b e = (b >>= \x -> step x e) == pure b
unsafeSwitchE :: B (E a) -> E (E a) -> E a
unsafeSwitchE (B i) (E e) = E $ switchEm (getE $ i 0) e 
  where switchEm :: [(Time,a)] -> [(Time,E a)] -> [(Time,a)]
        switchEm l [] = l 
        switchEm ((et,ev) : te) s@((st,_) : _) 
           | et < st = (et,ev) : switchEm te s
        switchEm _ ((st,e) : ts) = switchEm (forgetE e st) ts

-- proof obligation to use this function:
-- unsafeStep b e
-- assure that: hasUpdates b e = (b >>= \x -> step x e) == pure b
unsafeStep :: B a -> E a -> B a
unsafeStep b (E l) = b

-- derived

-- proof obligation:
-- hasUpdates b e
data Step a = Step { prev :: B a , updates :: E a } 

instance Functor Step where
  fmap f (Step b e) = step (fmap f b) (fmap f e)

step :: B a -> E a -> Step a
step b e = Step (unsafeStep b e) e

data LR a b = L a
            | R b
            | LR a b

unionLR :: E a -> E b -> E (LR a b)
unionLR l r = unionWith (\(L a) (R b) -> LR a b) (L <$> l) (R <$> r)

instance Applicative Step where
  pure x = Step (pure x) never
  (Step fb fe) <*> (Step xb xe) = step (fb <*> xb) ups where
     ups = observeE $ get <$> unionLR fe xe
     get (L f)    = f <$> xb
     get (R x)    = ($ x) <$> fb
     get (LR f x) = pure (f x)


switchS :: Step (E a) -> E a
switchS (Step p u) = unsafeSwitchE p u

joinStep :: Step (Step a) -> Step a
joinStep s@(Step i e) = step (i >>= prev) (switchS (updates <$> s))


-- derived from RE

apply :: B (a -> b) -> E a -> E b
apply f e = observeE $ fmap (\x -> ($ x) <$> f) e

accumE :: a -> E (a -> a) -> B (E a)
accumE i e = snd <$> (mfix $ \(~(b,_)) -> 
           do let e' = observeE $ (\f -> f <$> b) <$> e
              b <- stepb i e'
              return (b,e'))

switchE :: E (E a) -> B (E a)
switchE e = do b <- stepb never e
               return $ unsafeSwitchE b e


switchB :: B a -> E (B a) -> B (B a)
switchB b e = join <$> stepb b e       
         


