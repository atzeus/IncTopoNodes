{-# Language GeneralizedNewtypeDeriving #-} 

module PureEnv where
import Ex
import Data.Maybe
import IncTopoSort(Ex(..))
import Data.List
import Control.Monad.Fix

type Time = Int

newtype Pb a = Pb {runPb :: Time -> a } deriving (Functor,Applicative, Monad,MonadFix)
newtype Behavior a = B {runB :: Time -> a } deriving (Functor,Applicative, Monad,MonadFix)
newtype Events a = E {getE :: [Maybe a] }

data Emits a = Await  [Ex Events] (Behavior (EmitState a)) 
data EmitState a = Commit (Maybe a) (Emits a)

forget :: Time -> Events a -> [Maybe a]
forget t (E e) = drop t e

-- interface

-- primitives

liftPb :: Pb a -> Behavior a
liftPb (Pb f) = B f

isNow :: Events a -> Behavior (Maybe a)
isNow (E l) = B $ \t -> head $ drop t l

runEm :: Emits a -> Pb (Events a)
runEm e =  Pb $ \t -> E $ loop e t where
 loop (Await l m) t = 
     let l'      = map (\(Ex e) -> isJust <$> forget t e) l
         w       = getWait l'
         wl      = length w
         Commit em tt = runB m (t + wl)
         rest    = loop tt (t + wl + 1)
     in (Nothing <$ w) ++ (em : rest)

           
 getWait :: [[Bool]] -> [()]
 getWait [] = repeat ()
 getWait l  = () <$ takeWhile (all not) (transpose l)

hold :: a -> Events a -> Pb (Pb a)
hold i e = Pb $ \t -> Pb $ \t' -> scanl (fromMaybe) i (forget t e) !! (t' - t)

-- to prove :: 
unsafeRunPb :: Pb a -> a
unsafeRunPb (Pb f) = f 0
