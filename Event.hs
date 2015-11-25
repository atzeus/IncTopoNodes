module Event where

import EventBase
import System.IO.Unsafe
import Data.Monoid
import Control.Monad

first :: Event a -> Event a -> Event a
first l r = unsafePerformIO $ newFirst l r

bind :: Event a -> (a -> Event b) -> Event b
bind m f =  unsafePerformIO $ newBind m (return . f)

instance Monoid (Event a) where
  mempty = never
  mappend = first

instance Functor Event where fmap = liftM

instance Applicative Event where pure = return ; (<*>) = ap

instance Monad Event where
  return = at 0
  (>>=)  = bind




