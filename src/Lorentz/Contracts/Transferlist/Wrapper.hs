{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS -Wno-unused-do-bind #-}

module Lorentz.Contracts.Transferlist.Wrapper where

import Prelude hiding ((>>), drop, swap, get)
import GHC.Generics (Generic)
import Text.Show (Show(..))

import Lorentz
import Michelson.Typed.Haskell.Value (IsComparable)

import qualified Lorentz.Contracts.Transferlist as Transferlist
import qualified Lorentz.Contracts.Transferlist.Types as Transferlist
import qualified Lorentz.Contracts.Transferlist.Impl as Transferlist

type Entrypoint param store = '[ param, store] :-> ContractOut store

-- | Wrapper contract parameter type
data Parameter cp a
  = WrappedParameter !cp
  | TransferlistParameter !(Transferlist.Parameter' a)
  deriving  (Generic)

instance (HasTypeAnn cp, IsoValue cp) => ParameterHasEntryPoints (Parameter cp Address) where
  type ParameterEntryPointsDerivation (Parameter cp Address) = EpdPlain

deriving instance (Show cp, Show a) => Show (Parameter cp a)

deriving instance (IsoValue cp, IsoValue a) => IsoValue (Parameter cp a)

data Storage st a =
  Storage
    { wrappedStorage :: !st
    , transferlistStorage :: !(Transferlist.Storage a)
    }
  deriving  (Generic)

-- | Coerce from `Storage`
unStorage :: Storage st a & s :-> (st, Transferlist.Storage a) & s
unStorage = forcedCoerce_

-- | Coerce to `Storage`
toStorage :: (st, Transferlist.Storage a) & s :-> Storage st a & s
toStorage = forcedCoerce_

deriving instance (Show st, Show a) => Show (Storage st a)

deriving instance (IsoValue st, Ord a, IsoValue a, IsoCValue a)
  => IsoValue (Storage st a)

-- | Given a transformation from @cp@ to zero or one `Transferlist.TransferParams`,
-- use the transferlist to `Transferlist.assertTransfer` if `Just` and otherwise
-- execute the wrapped contract
transferlistWrappedContract :: forall a cp st. (IsComparable a, KnownValue a, NoOperation a, IsoValue cp)
  => (forall s. cp & s :-> Maybe (Transferlist.TransferParams a) & s)
  -> ContractCode cp st
  -> ContractCode (Parameter cp a) (Storage st a)
transferlistWrappedContract getTransferParams wrappedContract = do
  unpair
  caseT @(Parameter cp a)
    ( #cWrappedParameter /-> do
        dup
        getTransferParams
        ifNone
          (executeWrappedContract wrappedContract)
          (assertExecuteWrappedContract wrappedContract)
    , #cTransferlistParameter /-> do
        dip $ do
          unStorage
          unpair
        swap
        dip $ do
          pair
          Transferlist.transferlistManagementContract @a
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
-- after running `Transferlist.assertTransfer` on the given
-- `Transferlist.TransferParams`
assertExecuteWrappedContract :: forall a cp st. (NiceComparable a, IsComparable a)
  => ContractCode cp st
  -> Transferlist.TransferParams a & cp & '[Storage st a] :-> '[([Operation], Storage st a)]
assertExecuteWrappedContract wrappedContract = do
  dip $ do
    swap
    unStorage
    unpair
    swap
  Transferlist.assertTransfer
  iter Transferlist.assertReceivers
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
  swap -- the result of Transferlist.assertTransfer is a singleton or nothing
  prependSingletonOrNil
  pair

