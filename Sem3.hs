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

scanb :: (a →  b →  b) → b → Events a → Behavior b
scanb f i []            = \t -> i
scanb f i ((tv,v) : tt) = \t -> 
     if t <= tv then i else scanb f (f v i) tt t

scanb :: (a →  b →  b) → b → Events a → Behavior (Behavior b)
scanb f i e = \tstart -> go i estart where
     estart = [(t,v) | (t,v) <- e, t > tstart ]
     go :: b -> Events a -> Behavior b
     go i []           = \t -> i
     go i ((t,v) : tt)
         | 


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

unionWith :: (a -> a -> a) -> Events a -> Events a -> Events a
unionWith f [] r = r
unionWith f l [] = l
unionWith f l@((tl,vl) : lt) r@((tr,vr) : rt) 
      | tl <  tr  = (tl,vl)       : unionWith f lt r
      | tl == tr  = (tl, f vl vr) : unionWith f lt rt
      | tl >  tr  = (tr,vr)       : unionWith f l  rt

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

-- abstract data type, invariant:
-- assure that: hasUpdates b e = (b >>= \x -> stepb x e) == pure b
data Step a = Step (B a, E a)

prev :: Step a -> B a
prev (Step (b,e)) = b

updates :: Step a -> E a
updates (Step (b,e)) = e

-- proof obligation to use this function:
-- assure that: hasUpdates b e = (b >>= \x -> step x e) == pure b
unsafeStep :: B a -> E a -> Step a
unsafeStep b e = Step (b,e)

instance Functor Step where
  fmap f (Step (b,e)) = unsafeStep (fmap f b) (fmap f e)

data LR a b = L a
            | R b
            | LR a b

unionLR :: E a -> E b -> E (LR a b)
unionLR l r = unionWith (\(L a) (R b) -> LR a b) (L <$> l) (R <$> r)

instance Applicative Step where
  pure x = Step (pure x) never
  (Step fb fe) <*> (Step xb xe) = unsafeStep (fb <*> xb) ups where
     ups = observeE $ get <$> unionLR fe xe
     get (L f)    = f <$> xb
     get (R x)    = ($ x) <$> fb
     get (LR f x) = pure (f x)

joinStep :: Step (Step a) -> Step a
joinStep (Step (b,E e)) = Step (b >>= prev) (
  where switchEm :: [(Time,a)] -> [(Time,Step a)] -> [(Time,a)]
        switchEm l [] = l 
        switchEm ((et,ev) : te) s@((st,_) : _) 
           | et < st = (et,ev) : switchEm te s
        switchEm _ ((st,e) : ts) = switchEm (forgetE e st) ts


switchE :: Step (E a) -> E a
switchE (Step (b,E e)) = E (switchEm  
  where switchEm :: [(Time,a)] -> [(Time,E a)] -> [(Time,a)]
        switchEm l [] = l 
        switchEm ((et,ev) : te) s@((st,_) : _) 
           | et < st = (et,ev) : switchEm te s
        switchEm _ ((st,e) : ts) = switchEm (forgetE e st) ts

-- proof obligation to use this function:
-- switchEb b e
-- assure that: hasUpdates b e = (b >>= \x -> step x e) == pure b
unsafeSwitchE :: B (E a) -> E (E a) -> E a
unsafeSwitchE (B i) (E e) = E $ switchEm (getE $ i 0) e 








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
         


