module AllReal(never, filterJust, unionWith, observeE,  prev, updates, sampleS, joinB, switchE, justUp, justDown, scanlE, stepB,atn, scanlEM,
   Now, Events, Behavior, liftBehavior, NowIO, plan, callback,runFRP,combBN,timeNow,unsafeEndOfRound, unsafePrevIO, unsafePlan) where

import RealEnv
import Emits

unsafePrevIO :: Behavior a -> IO a
unsafePrevIO (Step (Pb m,_)) = m
