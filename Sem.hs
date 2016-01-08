{-# LANGUAGE GeneralizedNewtypeDeriving,DeriveFunctor,StandaloneDeriving, ViewPatterns #-}
module Sem where 
import Control.Monad
import Control.Monad.Fix
import Test.QuickCheck 
import Data.Maybe

-- not part of interface
type Time = Int

-- types of interface 

data B a = B {initv :: a, cs :: E a } deriving Functor

-- always infinite, starts at time == 0
newtype E a = E { getE :: [Maybe a] } deriving Functor

newtype M a = M { runM :: Time -> a } deriving (Functor,Applicative,Monad,MonadFix)

-- interface, types B,E,M abstract!

changes :: B a -> E a
changes = cs

never :: E a
never = E $ repeat Nothing

unionWith :: (a -> a -> a) -> E a -> E a -> E a
unionWith f (E l) (E r) = E $ zipWith combine l r
  where combine a b = (f <$> a <*> b) `mplus` a `mplus` b

filterJust :: E (Maybe a) -> E a
filterJust (E l) = E $ map join l

step :: a -> E a -> M (B a)
step i e = M $ \t -> B i (forgetE t e)

valueB :: B a -> M a
valueB b = M $ \time -> initv $ forgetB time b

isNow :: E a -> M (Maybe a)
isNow (E l) = M $ \time -> l !! time

observeE :: E (M a) -> E a 
observeE (E l) = E $ zipWith (\m t -> (`runM` t) <$> m) l [0..]

switchE :: B (E a) -> E a
switchE (B (E h) t) = E $ go h (getE $ forgetDiagE t) where
  go _ (Just (E (h : t)) : st) = h : go t st
  go (h : t) (Nothing : st)    = h : go t st

joinB :: B (B a) -> B a
joinB b@(B (B a ei) eo) = B a $ switchE (changes <$> b) `unionL` observeE (valueB <$> eo)
{-
joinB (B (B i ui) uo) = B i $ E $ go (getE ui) (getE $ forgetDiagB uo)
  where go :: [Maybe a] -> [Maybe (B a)] -> [Maybe a]
        go _ (Just (B hi (E (ei : t))) : st) = Just (fromMaybe hi ei) : go t st
        go (h : t) (Nothing : st)            = h : go t st
-}


-- derived

-- also follows from monad instance...

now :: B a -> M a
now x = do v <- isNow (changes x)
           case v of
            Just x -> pure x
            Nothing -> valueB x

instance Monad B where
  return = pure
  m >>= f = joinB (f <$> m)

instance Applicative B where
  pure x = B x never
  f@(B fh ft) <*> v@(B vh vt) = B (fh vh) t 
     where t = (coincide ft vt) `unionL` (f <@< vt) `unionL` (ft >@> v)

(<@<) :: B (a -> b) -> E a -> E b
b <@< e = observeE $ (\a -> ($ a) <$> valueB b) <$> e

(>@>) :: E (a -> b) -> B a -> E b
e >@> b = observeE $ (\f -> f <$> valueB b) <$> e 

data LR a b = L a | R b | LR a b deriving (Eq,Show)

unionL :: E a -> E a -> E a
unionL l r = unionWith (\x _ -> x) l r

unionLR :: E a -> E b -> E (LR a b)
unionLR l r = unionWith (\(L a) (R b) -> LR a b) (L <$> l) (R <$> r)

coincide :: E (a -> b) -> E a -> E b
coincide f v = filterJust $ applyEm <$> unionLR f v
  where applyEm (LR f v) = Just (f v)
        applyEm _        = Nothing

scanE :: (b -> a -> b) -> b -> E a -> M (B b)
scanE f i e = 
  mfix $ \b ->
    let em = (\v -> do i <- valueB b; pure (f i v)) <$> e
    in step i (observeE em)

edgeJust :: B (Maybe a) -> E a
edgeJust b = filterJust $ (edge <$> b) <@< changes b
  where edge Nothing (Just x) = Just x
        edge _        _       = Nothing


-- semantic functions
-- forget everything _before_ the given time
forgetE :: Time -> E a -> E a
forgetE t (E l) = E (replicate t Nothing ++ (drop t l))

forgetB :: Time -> B a -> B a
forgetB time (B i (E r)) =  B i' (forgetE time (E r))
  where i' = scanl fromMaybe i r !! time

forgetEDrop :: Time -> E a -> E a
forgetEDrop t (E l) = E (drop t l)

forgetBDrop :: Time -> B a -> B a
forgetBDrop time (B i (E r)) =  B i' (forgetEDrop time (E r))
  where i' = scanl fromMaybe i r !! time

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
  arbitrary = B <$> arbitrary <*> arbitrary
  shrink (B h t) = B h <$> shrink t


lengthE :: E a -> Int
lengthE (E l) = length l

lengthB :: B a -> Int
lengthB (B _ l) = lengthE l

trimE :: Int -> Int -> E a -> [Maybe a]
trimE t n (E l) = take (n - t) $ drop t l

trimB :: Int -> Int -> B a -> [a]
trimB t n (B h (E l)) = take (1 + n - t) $ drop t $ scanl fromMaybe h l

makeInfE :: E a -> E a
makeInfE (E l) = E (l ++ repeat Nothing)

makeInfB :: B a -> B a 
makeInfB (B h t) = B h (makeInfE t)

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

