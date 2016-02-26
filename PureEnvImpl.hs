{-# Language GeneralizedNewtypeDeriving, TypeFamilies #-}  
module PureEnvImpl where

import ImplInterface
import Ex
import Data.Maybe
import IncTopoSort(Ex(..))
import Data.List
import Control.Monad.Fix

type Time = Int

newtype PPb a       = Pb {runPb :: Time -> a } deriving (Functor,Applicative, Monad,MonadFix)
newtype PBehavior a = B  {runB :: Time -> a } deriving (Functor,Applicative, Monad,MonadFix)
newtype PEvents a   = E {getE :: [Maybe a] }

forget :: Time -> PEvents a -> [Maybe a]
forget t (E e) = drop t e

-- primitives

data Pure 

instance FRPImpl Pure where
  type Pb       Pure = PPb
  type Behavior Pure = PBehavior
  type Events   Pure = PEvents 
  liftPb (Pb f) = B f

  isNow (E l) = B $ \t -> head $ drop t l

  hold i e = Pb $ \t -> Pb $ \t' -> scanl (fromMaybe) i (forget t e) !! (t' - t)

  unsafeRunPb (Pb f) = f 0

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


