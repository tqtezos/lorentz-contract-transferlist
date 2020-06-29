{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS -Wno-unused-do-bind #-}

module Lorentz.Contracts.Filterlist.Impl where

import Prelude hiding ((>>), drop, swap, get)

import Lorentz
import Michelson.Text
import Michelson.Typed.Haskell.Value (IsComparable)

import Lorentz.Contracts.Filterlist.Types


--------------
-- Entrypoints
--------------


-- | Assert that one user is allowed to transfer to the other,
-- preserving the storage for additional calls.
--
-- The `issuer` is allowed to transfer to anyone.
--
-- If the sender's `FilterlistId`'s `OutboundFilterlists` is `unrestricted`,
-- they may transfer to any receiver whose `FilterlistId` is in their
-- `allowedFilterlists`.
assertTransfer_ ::
     forall a s. (NiceComparable a, IsComparable a)
  => TransferParams a & a & Users a & Filterlists & s :-> a & Users a & Filterlists & s
assertTransfer_ = do
  unTransferParams
  dip $ forcedCoerce_ @a @("issuer" :! a)
  forcedCoerce_ @(a, a) @("from" :! a, "to" :! a)
  swap
  stackType @("issuer" :! a & ("from" :! a, "to" :! a) & Users a & Filterlists & s)
  dip $ do
    dup
    dip $ do
      dip dup
      cdr
      forcedCoerce_ @("to" :! a) @a
      assertUserFilterlist
      forcedCoerce_ @FilterlistId @("to" :! FilterlistId)
    dup
    car
  stackType @("issuer" :! a & "from" :! a & ("from" :! a, "to" :! a) & "to" :! FilterlistId & Users a & Filterlists & s)
  dup
  dip $ do
    stackType @("issuer" :! a & "from" :! a & ("from" :! a, "to" :! a) & "to" :! FilterlistId & Users a & Filterlists & s)
    -- dipN @3 $ do
    dipN @4 $ do
      pair
      dup
      dip unpair
      unpair
    stackType @("issuer" :! a & "from" :! a & ("from" :! a, "to" :! a) & "to" :! FilterlistId & Users a & Filterlists & Users a & Filterlists & s)
    dip $ forcedCoerce_ @("from" :! a) @a
    forcedCoerce_ @("issuer" :! a) @a
    stackType @(a & a & ("from" :! a, "to" :! a) & "to" :! FilterlistId & Users a & Filterlists & Users a & Filterlists & s)
    ifEq -- 'from' user is issuer
      (do
        dropN @4
      )
      (do
        car
        dip $ do
          swap
        forcedCoerce_ @("from" :! a) @a
        assertUserFilterlist
        forcedCoerce_ @FilterlistId @("from" :! FilterlistId)
        swap
        dip $ do
          forcedCoerce_ @("from" :! FilterlistId) @FilterlistId
          assertOutboundFilterlists
          assertUnrestrictedOutboundFilterlists
        forcedCoerce_ @("to" :! FilterlistId) @FilterlistId
        mem
        stackType @(Bool & Users a & Filterlists & s)
        assert $ mkMTextUnsafe "outbound not filterlisted"
      )
    stackType @(Users a & Filterlists & s)
  forcedCoerce_ @("issuer" :! a) @a


-- | Run `assertTransfer_` once
assertTransfer ::
     forall a s. (NiceComparable a, IsComparable a)
  => TransferParams a & Storage a & s :-> ([Operation], Storage a) & s
assertTransfer = do
  dip $ do
    dup
    unStorage
    unpair
    dip car
    unpair
    -- issuer & users & filterlists & store
    stackType @(a & Users a & Filterlists & Storage a & s)
  assertTransfer_
  dropN @3
  nil
  pair

-- | `assertTransfer` for a list of `TransferParams`
assertTransfers ::
     forall a s. (NiceComparable a, IsComparable a)
  => [TransferParams a] & Storage a & s :-> ([Operation], Storage a) & s
assertTransfers = do
  dip $ do
    dup
    unStorage
    unpair
    dip $ do
      car
      dip nil
    unpair
  iter (assertTransfer_ @a)
  dropN @3
  pair

-- | Assert that all users are filterlisted and `unrestricted`, or the issuer
assertReceivers ::
     forall a s. (NiceComparable a, IsComparable a)
  => [a] & Storage a & s :-> ([Operation], Storage a) & s
assertReceivers = do
  dip $ do
    dup
    unStorage
    unpair
    dip car
    unpair
    stackType @(a & Users a & Filterlists & Storage a & s)
  iter assertReceiver
  dropN @3
  nil
  pair

-- | Set the issuer
--
-- Only admin
setIssuer :: forall a. () => Entrypoint a (Storage a)
setIssuer = do
  dip $ do
    unStorage
    unpair
    dip $ do
      unpair
      -- filterlists & admin
      stackType @(Filterlists & Address & '[])
      dip $ assertAdmin
      pair
    cdr
  pair
  pair
  toStorage
  nil
  pair

-- | Assert not equal with an error: @"issuer is not a user"@
assertNotIssuer :: (NiceComparable a, IsComparable a) => a & a & s :-> a & a & s
assertNotIssuer = do
  dup
  dip $ do
    dip dup
    assertNeq $ mkMTextUnsafe "issuer is not a user"

-- | Add/update a user with a particular `FilterlistId`,
-- or implicitly remove by providing `Nothing`
--
-- Only admin
updateUser :: forall a. (NiceComparable a, IsComparable a) => Entrypoint (UpdateUserParams a) (Storage a)
updateUser = do
  dip $ do
    unStorage
    unpair
    dip $ do
      unpair
      -- filterlists & admin
      stackType @(Filterlists & Address & '[])
      dip $ do
        assertAdmin
      pair
    unpair
  unUpdateUserParams
  -- user & new_filterlist & issuer & users & cdr store
  stackType @(a & Maybe FilterlistId & a & Users a & (Filterlists, Address) & '[])
  swap
  dip assertNotIssuer
  pair
  swap
  dip $ do
    unpair
    swap
    updateUserFilterlist
  pair
  pair
  toStorage
  nil
  pair

-- | Set the `FilterlistOutboundParams` for a `FilterlistId`
--
-- Only admin
setFilterlistOutbound :: forall a. () => Entrypoint FilterlistOutboundParams (Storage a)
setFilterlistOutbound = do
  dip $ do
    unStorage
    unpair
    swap
    unpair
    dip assertAdmin
  unFilterlistOutboundParams
  setOutboundFilterlists
  pair
  swap
  pair
  toStorage
  nil
  pair

-- | Set the admin `Address`
--
-- Only admin
setAdmin :: forall a. () => Entrypoint Address (Storage a)
setAdmin = do
  dip $ do
    unStorage
    unpair
    dip $ do
      unpair
      dip $ do
        assertAdmin_
  swap
  dip $ do
    swap
    pair
  pair
  toStorage
  nil
  pair


------------------
-- View parameters
------------------

-- | Get the issuer, who may be set by the admin and is unrestricted
getIssuer :: forall a. (NiceParameter a) => Entrypoint (View_ a) (Storage a)
getIssuer =
  view_ $ do
    cdr
    unStorage
    car
    car

-- | Get a user's `FilterlistId`, or `Nothing` if the user is not present
getUser :: forall a. (IsComparable a) => Entrypoint (View a (Maybe FilterlistId)) (Storage a)
getUser =
  view_ $ do
    unpair
    dip $ do
      unStorage
      car
      cdr
    getUserFilterlist

assertFilterlist :: forall a. () => Entrypoint AssertFilterlistParams (Storage a)
assertFilterlist = do
  unAssertFilterlistParams
  dip $ do
    dup
    unStorage
    cdr
    car
  unpair
  swap
  dip $ do
    outboundFilterlists
  ifNone
    (assertNone $ mkMTextUnsafe "exists")
    (do
      dip . assertSome $ mkMTextUnsafe "doesn't exist"
      assertSubsetOutboundFilterlists
    )
  nil @Operation
  pair

-- | Get the admin `Address` of the contract
getAdmin :: forall a. () => Entrypoint (View_ Address) (Storage a)
getAdmin =
  view_ $ do
    cdr
    unStorage
    cdr
    cdr


--------
-- Utils
--------

-- | Assert sender is the given address or fail with an error
assertAdmin_ :: Address & s :-> s
assertAdmin_ = do
  sender
  assertEq $ mkMTextUnsafe "only admin may update"

-- | `assertAdmin_`, but preserve the stack
assertAdmin :: Address & s :-> Address & s
assertAdmin = do
  dup
  dip assertAdmin_

