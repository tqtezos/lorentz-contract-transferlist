{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS -Wno-unused-do-bind #-}

module Lorentz.Contracts.Transferlist where

import Prelude hiding ((>>), drop, swap, get)
import GHC.Generics (Generic, Generic1)
import Text.Show (Show(..))

import Lorentz
import Michelson.Typed.Haskell.Value (IsComparable)
import Michelson.Typed.Value.Missing ()

import Lorentz.Contracts.Transferlist.Types
import Lorentz.Contracts.Transferlist.Impl

-- | Parameters are separated into assertions and management/`View`
-- parameters (`Parameter'`).
--
-- This helps facilitate using the code as different kinds of wrappers
-- while retaining a static type interface for the management/`View`
-- parameters.
data Parameter a
  -- | Assert that a list of transfers is valid
  = AssertTransfers ![TransferParams a]
  -- | Assert that a user is transferlisted and `unrestricted`
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


-- | Management parameters for the `transferlistContract`. See `Parameter'`.
transferlistManagementContract :: forall a. (IsComparable a, KnownValue a, NoOperation a)
  => ContractCode (Parameter' a) (Storage a)
transferlistManagementContract = do
  unpair
  caseT @(Parameter' a)
    ( #cSetIssuer /-> setIssuer
    , #cUpdateUser /-> updateUser
    , #cSetTransferlistOutbound /-> setTransferlistOutbound
    , #cSetAdmin /-> setAdmin
    , #cGetIssuer /-> getIssuer
    , #cGetUser /-> getUser
    , #cAssertTransferlist /-> assertTransferlist
    , #cGetAdmin /-> getAdmin
    )

-- | A contract that accepts either one or a batch of `TransferParams`
-- and throws and error if any transfers are disallowed.
--
-- Only transferlisted users and the "issuer" may participate in transfers.
-- - The issuer may transfer to any transferlisted user
-- - Each transferlisted user is associated to exactly one transferlist, by `TransferlistId`
-- - Each transferlist has a set of outbound transferlists that it may transfer to
-- - A transferlist may be restricted, i.e. its set of outbound transferlists is "empty"
--  * We store both @(`unrestricted` :: `Bool`)@ and
--    a set of `TransferlistId`'s so that restriction may be easily toggled
-- - A user may transfer to any other transferlisted user with a transferlist in their transferlist's outbound list
--
-- See `Parameter` and `Storage` for more detail.
transferlistContract :: forall a. (IsComparable a, KnownValue a, NoOperation a)
  => ContractCode (Parameter a) (Storage a)
transferlistContract = do
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
      transferlistManagementContract
      unpair
      nil @[a]
    )
  iter assertReceivers
  pair

