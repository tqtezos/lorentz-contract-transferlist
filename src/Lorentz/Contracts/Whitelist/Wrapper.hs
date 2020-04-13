{-# LANGUAGE RebindableSyntax #-}

module Lorentz.Contracts.Whitelist.Wrapper where

import Prelude hiding ((>>), drop, swap, get)
import GHC.Generics (Generic)
import Text.Show (Show(..))

import Lorentz
import Michelson.Typed.Haskell.Value (IsComparable)

import qualified Lorentz.Contracts.Whitelist as Whitelist
import qualified Lorentz.Contracts.Whitelist.Types as Whitelist
import qualified Lorentz.Contracts.Whitelist.Impl as Whitelist

type Entrypoint param store = '[ param, store] :-> ContractOut store

data Parameter cp a
  = WrappedParameter !cp
  | WhitelistParameter !(Whitelist.Parameter' a)
  deriving  (Generic)

instance (HasTypeAnn cp, IsoValue cp) => ParameterHasEntryPoints (Parameter cp Address) where
  type ParameterEntryPointsDerivation (Parameter cp Address) = EpdPlain

deriving instance (Show cp, Show a) => Show (Parameter cp a)

deriving instance (IsoValue cp, IsoValue a) => IsoValue (Parameter cp a)

data Storage st a =
  Storage
    { wrappedStorage :: !st
    , whitelistStorage :: !(Whitelist.Storage a)
    }
  deriving  (Generic)

unStorage :: Storage st a & s :-> (st, Whitelist.Storage a) & s
unStorage = forcedCoerce_

toStorage :: (st, Whitelist.Storage a) & s :-> Storage st a & s
toStorage = forcedCoerce_

deriving instance (Show st, Show a) => Show (Storage st a)

deriving instance (IsoValue st, Ord a, IsoValue a, IsoCValue a)
  => IsoValue (Storage st a)

-- | Given a transformation from @cp@ to zero or one `Whitelist.TransferParams`,
-- use the whitelist to `Whitelist.assertTransfer` if `Just` and otherwise
-- execute the wrapped contract
whitelistWrappedContract :: forall a cp st. (IsComparable a, KnownValue a, NoOperation a, IsoValue cp)
  => (forall s. cp & s :-> Maybe (Whitelist.TransferParams a) & s)
  -> ContractCode cp st
  -> ContractCode (Parameter cp a) (Storage st a)
whitelistWrappedContract getTransferParams wrappedContract = do
  unpair
  caseT @(Parameter cp a)
    ( #cWrappedParameter /-> do
        dup
        getTransferParams
        ifNone
          (executeWrappedContract wrappedContract)
          (assertExecuteWrappedContract wrappedContract)
    , #cWhitelistParameter /-> do
        dip $ do
          unStorage
          unpair
        swap
        dip $ do
          pair
          Whitelist.whitelistManagementContract @a
          unpair
        swap
        dip $ do
          pair
          toStorage
        pair
    )

-- | If the first argument has more than one element, it fails
-- with the second element, otherwise any elements are prepended
-- onto the second argument.
prependSingletonOrNil :: KnownValue a => [a] & [a] & s :-> [a] & s
prependSingletonOrNil = do
  ifCons
    (do
      swap
      dip cons
      ifCons
        failWith
        nop
    )
    nop

-- | Run the @wrappedContract@ on the given parameter and storage
executeWrappedContract :: forall a cp st. ()
  => ContractCode cp st
  -> cp & '[Storage st a] :-> '[([Operation], Storage st a)]
executeWrappedContract wrappedContract = do
  dip $ do
    unStorage
    unpair
    swap
  swap
  dip $ do
    pair
    wrappedContract
    unpair
  swap
  dip $ do
    swap
    pair
    toStorage
  pair

-- | Run the @wrappedContract@ on the given parameter and storage,
-- after running `Whitelist.assertTransfer` on the given
-- `Whitelist.TransferParams`
assertExecuteWrappedContract :: forall a cp st. (NiceComparable a, IsComparable a)
  => ContractCode cp st
  -> Whitelist.TransferParams a & cp & '[Storage st a] :-> '[([Operation], Storage st a)]
assertExecuteWrappedContract wrappedContract = do
  dip $ do
    swap
    unStorage
    unpair
    swap
  Whitelist.assertTransfer
  unpair
  dip $ do
    dip $ do
      swap
      pair
      wrappedContract
      unpair
      swap
    swap
    pair
    toStorage
    swap
  swap -- the result of Whitelist.assertTransfer is a singleton or nothing
  prependSingletonOrNil
  pair

