{-# OPTIONS -Wno-missing-export-lists #-}

module Lorentz.Contracts.SomeContractStorage where

import Lorentz
import Michelson.Typed.Scope


-- | Some contract storage with `SingI` and `HasNoOp` constraints
data SomeContractStorage where
  SomeContractStorage :: (SingI a, HasNoOp a)
    => Value a
    -> SomeContractStorage

-- | Run `SomeContractStorage`
fromSomeContractStorage :: forall b. SomeContractStorage -> (forall a. (SingI a, HasNoOp a) => Value a -> b) -> b
fromSomeContractStorage (SomeContractStorage xs) f = f xs

