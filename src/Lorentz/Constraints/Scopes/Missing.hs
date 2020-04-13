
module Lorentz.Constraints.Scopes.Missing where

import Michelson.Typed.Scope.Missing

import Michelson.Typed.Scope
import Michelson.Typed.Haskell.Value
import Lorentz.Constraints.Scopes

import Data.Constraint

-- type NiceComparable a = (KnownValue a, ProperComparabilityBetterErrors (ToT a))

-- +Lorentz.Constraints.Scope
niceComparableEvi :: forall a. NiceComparable a :- ComparabilityScope (ToT a)
niceComparableEvi =
  properComparabilityEvi @(ToT a) `trans` weaken2

-- -- proper
-- type NicePrintedValue a = (KnownValue a, ProperPrintedValBetterErrors (ToT a))

-- nicePrintedValueEvi :: forall a. NicePrintedValue a :- PrintedValScope (ToT a)
-- nicePrintedValueEvi =
--   properPrintedValEvi @(ToT a) `trans` weaken2

