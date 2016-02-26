{-# LANGUAGE GeneralizedNewtypeDeriving,DeriveFunctor,StandaloneDeriving, ViewPatterns #-}
module Sem where 
import Control.Monad
import Control.Monad.Fix
import Test.QuickCheck hiding (sample)
import Data.Maybe

-- not part of interface
type Time = Int

toTime :: RelTime -> Time
toTime One = 1
toTime (S x) = x + 1
-- 
type RelTime = One | S RelTime deriving (Eq, Ord)

relSub :: RelTime -> RelTime -> RelTime
relSub (S x) One   = x
relSub (S x) (S y) = relSub x y

relAdd :: RelTime -> RelTime -> RelTime
relAdd One One = S One
relAdd (S x) r = S (relAdd x r)

addRel :: Time -> RelTime -> Time
addRel x One   = x + 1
addRel x (S n) = (addRel x n) + 1

-- types of interface 

data B a = B a (E a) deriving Functor
data E a = Never
         | E RelTime (B a) deriving Functor

newtype M a = M { runM :: Time -> a } deriving (Functor,Applicative,Monad,MonadFix)

advance ::  RelTime -> E a -> E a
advance t (E tl b) = E (relSub tl t) b

delay :: RelTime -> E a -> E a
delay t (E tl b) = E (relAdd tl t) b

forget :: Time -> E a -> E a
forget t e@(E rt (B h tt)) 
  | t < toTime rt = e
  | otherwise     = 

-- interface, types B,E,M abstract!

never :: E a
never = Never

always :: a -> B a
always x = B x Never

unionWith :: (a -> a -> a) -> E a -> E a -> E a
unionWith f l@(E tl (B lv lt)) r@(E tr (B rv rt)) = 
     case compare tl tr of
       Eq -> E tl $ B (f lv rv) $ unionWith f lt rt
       Lt -> E tl $ B lv        $ unionWith f lt (advance tl r)
       Gt -> E tr $ B rv        $ unionWith f (advance tr l) rt

filterJust :: E (Maybe a) -> E a
filterJust (E t (B (Just x) tt)) = E t (B x (filterJust tt))
filterJust (E t (B Nothing  tt)) = delay t $ filterJust tt 

sample :: B a -> M a
sample (B h (E t tt)) = M $ \time -> 
  if time < toTime t then h else runM (sample tt) (time - toTime t)

prev :: B a -> M a
prev (B h (E t tt)) = M $ \time -> 
  if time <= toTime t then h else runM (sample tt) (time - toTime t)

isNow :: E a -> M (Maybe a)
isNow (E t (B h tt)) = M $ \time -> 
  case compare time (toTime t) of
    Lt -> Nothing
    Eq -> Just h
    Gt -> runM (isNow tt) (time - toTime t)

observeE :: E (M a) -> E a 
observeE l = uncurry runM <$> withTime 0 l
  where withTime :: Time -> E a -> E (a,Time)
        withTime t (E rt (B h tt)) = 
            let t' = addRel t rt
            in E rt (B (h,t') (withTime t' tt)
                                        

joinB :: B (B a) -> B a
joinB (B b) = B $ zipWith (\(B l) t -> l !! t) b [0..]

class Monad b => Sampleable b where
  step    :: a -> E a -> M (b a)
  sample  :: b a     -> M a
  switchE :: b (E a) -> E a

instance Sampleable B where
  step i e      = M $ \t -> B $ scanl fromMaybe i (getE $ forgetE t e)
  sample (B b)  = M $ \time -> b !! time
  switchE (B b) = E $ zipWith (\(E l) t -> l !! t) b [0..]

-- composed loss of sharing...

data Step a = Step { prev :: B a, updates :: E a } deriving Functor

joinStep :: Step (Step a) -> Step a
joinStep (Step b e) = Step (joinB (prev <$> b)) (coincide (updates <$> e))

justEdgeUp :: Step (Maybe a) -> E a
justEdgeUp (Step b e) = filterJust $ goesUp <$> b <@< e
  where goesUp Nothing (Just x) = Just x
        goedUp _       _        = Nothing

justEdgeDown :: Step (Maybe a) -> E a
justEdgeDown (Step b e) = filterJust $ goesDown <$> b <@< e
  where goesDown (Just x) Nothing = Just x
        goesDown _       _        = Nothing

instance Sampleable Step where
  step i e = Step <$> step i e <*> pure e
  sample (Step b e) = do x <- isNow e
                         case x of
                           Just v  -> return v
                           Nothing -> sample b
  switchE (Step b e) = coincide e `unionL` switchE b
  


instance Applicative Step where
  pure x = Step (pure x) never 
  (Step f fe) <*> (Step x xe) = Step (f <*> x) (coincide2 fe xe `unionL` (f <@< xe) `unionL` (fe >@> x))
 

instance Monad Step where
  return  = pure 
  m >>= f = joinStep (fmap f m)


-- derived


instance Monad B where
  return = pure
  m >>= f = joinB (f <$> m)

-- also follows from monad instance...
instance Applicative B where
  pure = always
  (B f) <*> (B x) = B (zipWith ($) f x)

infixl 4 <@<
(<@<) :: Sampleable b => b (x -> y) -> E x -> E y
b <@< e = observeE $ (\a -> ($ a) <$> sample b) <$> e

infixl 4 >@>
(>@>) ::  Sampleable b => E (x -> y) -> b x -> E y
e >@> b = observeE $ (\f -> f <$> sample b) <$> e 

data LR a b = L a | R b | LR a b deriving (Eq,Show)

unionL :: E a -> E a -> E a
unionL l r = unionWith (\x _ -> x) l r

unionLR :: E a -> E b -> E (LR a b)
unionLR l r = unionWith (\(L a) (R b) -> LR a b) (L <$> l) (R <$> r)


coincide :: E (E a) -> E a
coincide l = filterJust $ observeE $ isNow <$> l

coincide2 :: E (a -> b) -> E a -> E b
coincide2 f v = filterJust $ applyEm <$> unionLR f v
  where applyEm (LR f v) = Just (f v)
        applyEm _        = Nothing

scanE :: (b -> a -> b) -> b -> E a -> M (B b)
scanE f i e = 
  mfix $ \b ->
    let em = (\v -> do i <- sample b; pure (f i v)) <$> e
    in step i (observeE em)

{-
edgeJust :: B (Maybe a) -> E a
edgeJust b = filterJust $ (edge <$> b) <@< changes b
  where edge Nothing (Just x) = Just x
        edge _        _       = Nothing
-}

-- semantic functions
-- forget everything _before_ the given time
forgetE :: Time -> E a -> E a
forgetE t (E l) = E (replicate t Nothing ++ (drop t l))

forgetB :: Time -> B a -> B a
forgetB t (B l) =  B (replicate t undefined ++ (drop t l))

forgetEDrop :: Time -> E a -> E a
forgetEDrop t (E l) = E (drop t l)

forgetBDrop :: Time -> B a -> B a
forgetBDrop t (B l) =  B (drop t l)

forgetDiagB :: E (B a) -> E (B a)
forgetDiagB (E t) = E $ zipWith (\t i -> forgetBDrop t <$> i)  [0..] t

forgetDiagE :: E (E a) -> E (E a)
forgetDiagE (E t) = E $ zipWith (\t i -> forgetEDrop t <$> i)  [0..] t


--- testing stuff

interpret :: (E a -> M (E b)) -> [Maybe a] -> [Maybe b]
interpret f l = 
  let E x = f (E $ l ++ repeat Nothing) `runM` 0
  in take (length l) x

interpretB :: (E a -> M (B b)) -> [Maybe a] -> [b]
interpretB f l = 
  let x = f (E $ l ++ repeat Nothing) `runM` 0
  in trimB 0 (length l) x


deriving instance Show a => Show (B a)
deriving instance Show a => Show (E a)
deriving instance Arbitrary a => Arbitrary (E a)

instance Arbitrary a => Arbitrary (B a) where
  arbitrary = B <$> arbitrary
  shrink (B l) = B <$> shrink l


lengthE :: E a -> Int
lengthE (E l) = length l

lengthB :: B a -> Int
lengthB (B l) = length l

trimE :: Int -> Int -> E a -> [Maybe a]
trimE t n (E l) = take (n - t) $ drop t l

trimB :: Int -> Int -> B a -> [a]
trimB t n (B l) = take (1 + n - t) $ drop t  l

makeInfE :: E a -> E a
makeInfE (E l) = E (l ++ repeat Nothing)

makeInfB :: B a -> B a 
makeInfB (B l) = B $ l ++ repeat (head (reverse l))

makeInfBB :: B (B a) -> B (B a) 
makeInfBB b =  makeInfB <$> makeInfB b

makeInfBE :: B (E a) -> B (E a)
makeInfBE b =  makeInfE <$> makeInfB b


-- properties of semantics

-- forgetfull:

prop_filterForget :: E (Maybe Int) -> Property
prop_filterForget l' = 
  let n = lengthE l' 
      l = makeInfE l'
  in forAll (choose (0,n)) $ \t ->
     let forget = forgetE t
         l =?= r = trimE t n l === trimE t n r
     in forget (filterJust l) =?= filterJust (forget l)


prop_unionForget :: E Int -> E Int -> Property
prop_unionForget l' r' =
  let n = max (lengthE l') (lengthE r')
      l = makeInfE l'
      r = makeInfE r'
  in forAll (choose (0,n)) $ \t ->
     let forget = forgetE t
         l =?= r = trimE t n l === trimE t n r
     in forget (unionLR l r) =?= unionLR (forget l) (forget r) 



prop_switchEForget :: B (E Int) -> Property
prop_switchEForget b' = 
  let n = lengthB b' 
      b = makeInfB (makeInfE <$> b')
  in forAll (choose (0,n)) $ \t ->
     let forgetb = forgetB t
         forgete = forgetE t
         l =?= r = trimE t n l === trimE t n r
     in forgete (switchE b) =?= switchE (forgetb b)

prop_joinForget :: B (B Int) -> Property
prop_joinForget b' = 
  let n = lengthB b' 
      b = makeInfB (makeInfB <$> b')
  in forAll (choose (0,n)) $ \t ->
     let forget = forgetB t
         l =?= r = trimB t n l === trimB t n r
     in forget (join b) =?= join (forget b)


-- monad and applicative agree

prop_apIsAp :: B Int -> B Int -> Property
prop_apIsAp l' r' =
  let n = max (lengthB l') (lengthB r')
      l = makeInfB l'
      r = makeInfB r'
      l =?= r = trimB 0 n l === trimB 0 n r
  in ((,) <$> l <*> r) =?= (return (,) `ap` l `ap` r)

