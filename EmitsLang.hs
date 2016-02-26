module EmitsLang where

import FList 

data Await m a where
  Await  :: FList E l -> (FList Maybe l -> m (EmitState a)) -> Await a

data EmitState m a = WaitMore (Await m a)
                   | Commit (Maybe a) (Await m a)

type Emits m a = m (Await a)
