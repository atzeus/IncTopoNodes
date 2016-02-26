module FRPLib where

import AllReal
import Control.Monad.Fix
import Control.Monad.IO.Class
import Debug.Trace

class Applicative b => Sampleable b where
  sample :: b a -> Now a

unionl :: Events a -> Events a -> Events a
unionl a b = unionWith (\x y -> x) a b

infixl 4 <@
(<@) :: Behavior x -> Events y -> Events x
b <@ e = (const <$> sample b) `atn` e

infixl 4 <@<
(<@<) ::  Behavior (x -> y) -> Events x -> Events y
b <@< e = sample b `atn` e

infixl 4 <<@<
(<<@<) ::  Now (x -> y) -> Events x -> Events y
b <<@< e = sample b `atn` e

infixl 4 >@>>
(>@>>) :: Events (x -> y) -> Now x -> Events y
e >@>> b = ((flip ($)) <$> sample b) `atn` e

infixl 4 >@>
(>@>) :: Events (x -> y) -> Behavior x -> Events y
e >@> b = ((flip ($)) <$> sample b) `atn` e

at :: Behavior x -> Events y -> Events x
at = (<@)

instance Sampleable Now where
  sample = id

instance Sampleable Behavior where
  sample = sampleS

when :: Behavior Bool -> Events ()
when b = () <$ filtere id (edgeUp <$> prev b <<@< updates b) where
   edgeUp False True = True
   edgeUp _ _ = False

infixl 4 <*>.
(<*>.) :: Behavior (a -> b) -> Behavior a -> Behavior b
f <*>. x = combBN f x

infixl 4 .<*>
(.<*>) :: Behavior (a -> b) -> Behavior a -> Behavior b
f .<*> x = combBN (flip ($) <$> x) f


during :: Events a -> Behavior Bool -> Events a
during e b = filterJust $ choose <$> b <@< e
 where choose True = Just
       choose False = const Nothing

dynList :: Events (Behavior a) -> Now (Behavior [a])
dynList e = joinB <$> scan (\l h -> (:) <$> h <*> l) (pure []) e

b :: Now a -> NowIO a
b = liftBehavior

union = unionl

class Monad b => BehaviorLike b where
  liftB :: Now a -> b a

{-
scans :: BehaviorLike b =>  (x -> y -> x) -> x -> Behavior y -> b (Behavior x)
scans f i s = do h <- liftB $ prev s
                 let i' = f i h
                 scan f i' (updates s)
-}

hold :: BehaviorLike b => a -> Events a -> b (Behavior a)
hold i e = liftB $ stepB i e

scan :: (x -> y -> x) -> x -> Events y -> Now (Behavior x)
scan f i e = liftB $ scanlE f i e

step :: a -> Events a -> Now (Behavior a)
step i e = liftB $ stepB i e

scanM = scanlEM


instance BehaviorLike Now where
  liftB = id

instance BehaviorLike NowIO where
  liftB = liftBehavior

filtere :: (a -> Bool) -> Events a -> Events a
filtere f e = filterJust $ test <$> e  where
  test a = if f a then Just a else Nothing

-- The history consists of time/position pairs (and is never null)
type History a = [(Double,a)]
type Duration = Double

fixStep :: MonadFix m => (Now a -> m (Behavior a)) -> m (Behavior a)
fixStep f = mfix (f . prev)

{-
buffer :: BehaviorLike b => Duration -> Step Time -> Events a -> b (Step (History a))
buffer dur t s =
  do let tagged = (,) <$> t <@< s
     let ups = (Left <$> tagged) `unionl` (Right <$> updates t)
     r <- scan act [] ups
     return (reverse <$> r)
 where act l (Left (t,v)) = (t,v) : prune t l
       act l (Right t)    = prune t l
       prune t l =  takeWhile before l where
        cutoffTime = t - dur
        before (tv,_) = tv >= cutoffTime
-}
traceEvs :: Show a => Events a -> Now x -> Now x
traceEvs evs b =
    do unsafePlan (liftIO . traceIO . show <$> evs)
       b

{-
remember :: Duration -> Step a -> Goey (Step (History a))
remember dur s =
  do now <- time
     init <- snd <$> sample s
     scan addPrune [(now,init)] (updates s)
 where addPrune l (t,v) = (t,v) : takeWhile before l where
        cutoffTime = t - dur
        before (tv,_) = tv >= cutoffTime

getTimeStep :: Events x -> Behavior (Step Time)
getTimeStep e = do t <- time
                   step t (time <@ e)
-}
--newtype Bt a =
