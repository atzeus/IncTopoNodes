{-# Language GADTs #-} 

module Ex where

{-| An @Ex f@ is the GADT encoding of the type @exists a. f a@
-}
data Ex f where
  Ex :: f a -> Ex f
