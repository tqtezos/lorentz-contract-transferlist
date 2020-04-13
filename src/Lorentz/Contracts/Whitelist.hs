{-# LANGUAGE RebindableSyntax #-}

module Lorentz.Contracts.Whitelist where

import Prelude hiding ((>>), drop, swap, get)
import GHC.Generics (Generic, Generic1)
import Text.Show (Show(..))

import Lorentz
import Michelson.Typed.Haskell.Value (IsComparable)
import Michelson.Typed.Value.Missing ()

import Lorentz.Contracts.Whitelist.Types
import Lorentz.Contracts.Whitelist.Impl

-- | Parameters are separated into assertions and management/`View`
-- parameters (`Parameter'`).
--
-- This helps facilitate using the code as different kinds of wrappers
-- while retaining a static type interface for the management/`View`
-- parameters.
data Parameter a
  -- | Assert that a transfer is valid
  = AssertTransfer !(TransferParams a)
  -- | Assert that a user is whitelisted and `unrestricted`
  | AssertReceiver !a
  -- | Assert that users are whitelisted and `unrestricted`
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


-- | A contract that accepts either one or a batch of `TransferParams`
-- and throws and error if any transfers are disallowed.
--
-- Only whitelisted users and the "issuer" may participate in transfers.
-- - The issuer may transfer to any whitelisted user
-- - Each whitelisted user is associated to exactly one whitelist, by `WhitelistId`
-- - Each whitelist has a set of outbound whitelists that it may transfer to
-- - A whitelist may be restricted, i.e. its set of outbound whitelists is "empty"
--  * We store both @(`unrestricted` :: `Bool`)@ and
--    a set of `WhitelistId`'s so that restriction may be easily toggled
-- - A user may transfer to any other whitelisted user with a whitelist in their whitelist's outbound list
--
-- See `Parameter` and `Storage` for more detail.
whitelistContract :: forall a. (IsComparable a, KnownValue a, NoOperation a)
  => ContractCode (Parameter a) (Storage a)
whitelistContract = do
  unpair
  -- since AssertReceiver is just AssertReceivers with a singleton list,
  -- we wrap the three cases into: Either (Either transfer other) receiver
  caseT @(Parameter a)
    ( #cAssertTransfer /-> left >> left
    , #cAssertReceiver /-> dip nil >> cons >> right
    , #cAssertReceivers /-> right
    , #cOtherParameter /-> right >> left
    )
  ifLeft
    (ifLeft
      assertTransfer
      (pair >> whitelistManagementContract)
    )
    assertReceivers

-- | Management parameters for the `whitelistContract`. See `Parameter'`.
whitelistManagementContract :: forall a. (IsComparable a, KnownValue a, NoOperation a)
  => ContractCode (Parameter' a) (Storage a)
whitelistManagementContract = do
  unpair
  caseT @(Parameter' a)
    ( #cSetIssuer /-> setIssuer
    , #cAddUser /-> addUser
    , #cSetWhitelistOutbound /-> setWhitelistOutbound
    , #cSetAdmin /-> setAdmin
    , #cGetIssuer /-> getIssuer
    , #cGetUser /-> getUser
    , #cGetWhitelist /-> getWhitelist
    , #cGetAdmin /-> getAdmin
    )

