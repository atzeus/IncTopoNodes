{-# Language RankNTypes,TypeOperators, DataKinds,FunctionalDependencies,TypeSynonymInstances, MultiParamTypeClasses,ScopedTypeVariables, GADTs,GeneralizedNewtypeDeriving, ViewPatterns, TupleSections #-} 

import Control.Monad.Writer
import Control.Monad.State
import Data.List
import Data.Maybe
import FList 


type Time = Int

newtype B a = B {runB :: Time -> a } deriving (Functor,Applicative, Monad,MonadFix)
newtype E a = E {getE :: [Maybe a] }

data Emits a where
  Await  :: FList E l -> (FList Maybe l -> B (EmitState a)) -> Emits a

data EmitState a = WaitMore (Emits a)
                 | Commit (Maybe a) (Emits a)




nevere :: Emits a
nevere = Await X undefined

fmape :: (x -> y) -> E x -> Emits y
fmape f e = loop where
 loop = Await (e :. X) $ \(Just x :. X) ->
      pure $ Commit (Just (f x)) loop

filterJust :: E (Maybe a) -> Emits a
filterJust e = loop where
  loop = Await (e :. X) $ \(x :. X) ->
          pure $ Commit (join x) loop

unionWith :: (a -> a -> a) -> E a -> E a -> Emits a 
unionWith f l r = loop where
 loop = Await (l :. r :. X) $ \(lv :. rv :. X) ->
         pure (Commit (Just $ combine lv rv) loop)
 combine (Just x) (Just y) = f x y
 combine Nothing  (Just y) = y
 combine (Just x) Nothing  = x
  
observeE :: E (B a) -> Emits a
observeE e = loop where
 loop = Await (e :. X) $ \(Just xm :. X) ->
         do x <- xm
            pure $ Commit (Just x) loop


switchE :: E a -> E (E a) -> Emits a 
switchE b e = go b where
 go i = Await (i :. e :. X) $ \(iv :. ev :. X) ->
       pure $ case ev of
         Just i' -> WaitMore $ Await (i' :. e :. X) $ \(iv' :. _ :. X) ->
                            pure $ Commit iv' (go i')
         Nothing -> Commit iv (go i)




stepb :: a -> E a -> B (B a)
stepb i (E l) = B $ \t -> B $ \t' -> (scanl fromMaybe i (drop t l)) !! (t' - t)

-- proof obligation : b == const x
unsafeGetBe :: B (E a) -> E a
unsafeGetBe b = runB b 0
{-
runEm :: Emits a -> B (E a)
runEm e =  B $ \t -> 
        E $ replicate t Nothing ++ loop (runB e t) t where
  
 loop (Await l m) t = 
     let w       = getWait l t
         wl      = length w
         (em,tt) = runB m (t + wl)
         rest    = loop tt (t + wl + 1)
     in w ++ (em : rest)
     
forget :: Time -> E a -> E a
forget t (E e) = E (drop t e)

getHead :: E a -> Maybe a
getHead (E e) = head a


            
getWait :: [[Bool]] -> Time -> [Maybe a]
getWait [] _ = repeat Nothing
getWait l t  = Nothing <$ takeWhile (all not) (transpose l')
-}
