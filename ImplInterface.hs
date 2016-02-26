{-# Language MultiParamTypeClasses, TypeFamilies #-} 
module ImplInterface where
import Ex

data Emits i a = Await  [Ex (Events i)] (Behavior i (EmitState i a)) 
data EmitState i a = Commit (Maybe a) (Emits i a)

class FRPImpl i where
  type Pb       i :: * -> *
  type Behavior i :: * -> *
  type Events   i :: * -> *
  liftPb      :: Pb i a -> Behavior i a
  isNow       :: Events i a -> Behavior i (Maybe a)
  runEm       :: Emits i a -> Pb i (Events i a)
  hold        :: a -> Events i a -> Pb i (Pb i a)
  unsafeRunPb :: Pb i a -> a

