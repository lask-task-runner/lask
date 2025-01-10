module Language.Lask.Utils
  ( tupleToCofree,
    Pretty (..),
  )
where

import Control.Comonad.Cofree

tupleToCofree :: (a, f (Cofree f a)) -> Cofree f a
tupleToCofree (s, v) = s :< v

class Pretty a where
  pretty :: a -> String
