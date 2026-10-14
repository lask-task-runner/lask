-- | The operators @QualifiedDo@ uses for "Language.Lask.Embed":
--
-- > {-# LANGUAGE QualifiedDo #-}
-- > import qualified Language.Lask.Embed.Do as L
-- >
-- > body = L.do
-- >   tag <- run git "describe --tags"   -- x = e: runs once, shared
-- >   run go ("build -ldflags=-X=main.version=" <> tag)
-- >   "built " <> tag
--
-- @x <- e@ is a binding of a Lask @do@ block, a statement on its own is
-- run for its effects, and the last statement is the block's value.
module Language.Lask.Embed.Do ((>>=), (>>)) where

import GHC.Stack (HasCallStack, callStack)
import Language.Lask.Embed.Internal (E, bindE, callSpan, thenE)
import Prelude hiding ((>>), (>>=))

-- | @x <- e@: bind the result of @e@ once, for the rest of the block.
(>>=) :: (HasCallStack) => E v a -> (E v a -> E v b) -> E v b
(>>=) = bindE (callSpan callStack)

-- | A statement run for its effects, then the rest of the block.
(>>) :: (HasCallStack) => E v a -> E v b -> E v b
(>>) = thenE (callSpan callStack)
