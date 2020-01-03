{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-unused-do-bind -Wno-partial-fields -Wno-orphans #-}

module Lorentz.Contracts.Whitelist.Wrapper where

import Prelude hiding ((>>), drop, swap, get)
import GHC.Generics (Generic)
import Text.Show (Show(..))
import Text.Read (Read(..))

import Lorentz
import Michelson.Typed.Haskell.Value (IsComparable)
import Michelson.Typed.Scope

-- import Data.Functor.Timestamped
import Lorentz.Contracts.Util ()
import qualified Lorentz.Contracts.Whitelist as Whitelist

type Entrypoint param store = '[ param, store] :-> ContractOut store

data Parameter cp a
  = WrappedParameter
      { wrappedParams :: !cp
      }
  | WhitelistParameter
      { whitelistParams :: !(Whitelist.Parameter' a)
      }
  deriving  (Generic)

instance (NiceParameter cp, HasNoOp (ToT cp), HasNoNestedBigMaps (ToT cp), NiceParameter a) => ParameterEntryPoints (Parameter cp a) where
  parameterEntryPoints = pepNone

deriving instance (Read cp, NiceParameter a, Read a) => Read (Parameter cp a)

deriving instance (Show cp, Show a) => Show (Parameter cp a)

deriving instance (IsoValue cp, IsoValue a) => IsoValue (Parameter cp a)

data Storage st a =
  Storage
    { wrappedStorage :: !st
    , whitelistStorage :: !(Whitelist.Storage a)
    }
  deriving  (Generic)

unStorage :: Storage st a & s :-> (st, Whitelist.Storage a) & s
unStorage = coerce_

toStorage :: (st, Whitelist.Storage a) & s :-> Storage st a & s
toStorage = coerce_

deriving instance (Show st, Show a) => Show (Storage st a)

deriving instance (IsoValue st, Ord a, IsoValue a, IsoCValue a) => IsoValue (Storage st a)

whitelistWrappedContract :: forall a cp st. (IsComparable a, CompareOpHs a, Typeable a, KnownValue a, NoOperation a, IsoValue cp)
  => (forall s. cp & s :-> Maybe (Whitelist.TransferParams a) & s)
  -> Contract cp st
  -> Contract (Parameter cp a) (Storage st a)
whitelistWrappedContract getTransferParams wrappedContract = do
  unpair
  caseT @(Parameter cp a)
    ( #cWrappedParameter /-> do
        dup
        getTransferParams
        ifNone
          (do
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
          )
          (do
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
          )
        -- wrappedParameter
    , #cWhitelistParameter /-> do
        dip $ do
          unStorage
          unpair
        swap
        dip $ do
          caseT @(Whitelist.Parameter' a)
            ( #cSetIssuer /-> Whitelist.setIssuer
            , #cAddUser /-> Whitelist.addUser
            , #cSetWhitelistOutbound /-> Whitelist.setWhitelistOutbound
            , #cSetAdmin /-> Whitelist.setAdmin
            , #cGetIssuer /-> Whitelist.getIssuer
            , #cGetUser /-> Whitelist.getUser
            , #cGetWhitelist /-> Whitelist.getWhitelist
            , #cGetAdmin /-> Whitelist.getAdmin
            )
          unpair
        swap
        dip $ do
          pair
          toStorage
        pair
    )

-- | Appends the first list backwards onto the second
--
-- For this case, the list will always have 0 or 1 elements
-- but this is easier than proving it
{-# DEPRECATED appendList "Convenient waste of gas" #-}
appendList :: forall a s. KnownValue a => [a] & [a] & s :-> [a] & s
appendList = do
  unconsEither
  loopLeft $ do
    unpair
    swap
    dip cons
    unconsEither
  drop
  where
    unconsEither :: forall s'. [a] & s' :-> Either (a, [a]) () & s'
    unconsEither = do
      ifCons
        (do
          pair
          left
        )
        (do
          unit
          right
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

