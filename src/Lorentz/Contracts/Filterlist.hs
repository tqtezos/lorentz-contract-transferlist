{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS -Wno-unused-do-bind #-}

module Lorentz.Contracts.Filterlist where

import Prelude hiding ((>>), drop, swap, get)
import GHC.Generics (Generic, Generic1)
import Text.Show (Show(..))

import Lorentz
import Michelson.Typed.Haskell.Value (IsComparable)
import Michelson.Typed.Value.Missing ()

import Lorentz.Contracts.Filterlist.Types
import Lorentz.Contracts.Filterlist.Impl

-- | Parameters are separated into assertions and management/`View`
-- parameters (`Parameter'`).
--
-- This helps facilitate using the code as different kinds of wrappers
-- while retaining a static type interface for the management/`View`
-- parameters.
data Parameter a
  -- | Assert that a list of transfers is valid
  = AssertTransfers ![TransferParams a]
  -- | Assert that a user is filterlisted and `unrestricted`
  | AssertReceivers ![a]
  -- | Management and `View` parameters
  | OtherParameter !(Parameter' a)
  deriving (Generic, Generic1)

-- instance (HasTypeAnn a, IsoValue a) => ParameterHasEntryPoints (Parameter a) where
--   type ParameterEntryPointsDerivation (Parameter a) = EpdRecursive
instance ParameterHasEntryPoints (Parameter Address) where
  type ParameterEntryPointsDerivation (Parameter Address) = EpdRecursive

deriving instance Show a => Show (Parameter a)

deriving instance IsoValue a => IsoValue (Parameter a)


-- | Management parameters for the `filterlistContract`. See `Parameter'`.
filterlistManagementContract :: forall a. (IsComparable a, KnownValue a, NoOperation a)
  => ContractCode (Parameter' a) (Storage a)
filterlistManagementContract = do
  unpair
  caseT @(Parameter' a)
    ( #cSetIssuer /-> setIssuer
    , #cUpdateUser /-> updateUser
    , #cSetFilterlistOutbound /-> setFilterlistOutbound
    , #cSetAdmin /-> setAdmin
    , #cGetIssuer /-> getIssuer
    , #cGetUser /-> getUser
    , #cAssertFilterlist /-> assertFilterlist
    , #cGetAdmin /-> getAdmin
    )

-- | A contract that accepts either one or a batch of `TransferParams`
-- and throws and error if any transfers are disallowed.
--
-- Only filterlisted users and the "issuer" may participate in transfers.
-- - The issuer may transfer to any filterlisted user
-- - Each filterlisted user is associated to exactly one filterlist, by `FilterlistId`
-- - Each filterlist has a set of outbound filterlists that it may transfer to
-- - A filterlist may be restricted, i.e. its set of outbound filterlists is "empty"
--  * We store both @(`unrestricted` :: `Bool`)@ and
--    a set of `FilterlistId`'s so that restriction may be easily toggled
-- - A user may transfer to any other filterlisted user with a filterlist in their filterlist's outbound list
--
-- See `Parameter` and `Storage` for more detail.
filterlistContract :: forall a. (IsComparable a, KnownValue a, NoOperation a)
  => ContractCode (Parameter a) (Storage a)
filterlistContract = do
  unpair
  caseT @(Parameter a)
    ( #cAssertTransfers /-> assertTransfers
    , #cAssertReceivers /-> do
      dip $ do
        nil @Operation
        nil @[a]
      cons
    , #cOtherParameter /-> do
      pair
      filterlistManagementContract
      unpair
      nil @[a]
    )
  iter assertReceivers
  -- nil @Operation
  pair

