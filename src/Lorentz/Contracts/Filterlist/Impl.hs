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
  => TransferParams a & a & [[a]] & Users a & Filterlists & s :-> a & [[a]] & Users a & Filterlists & s
assertTransfer_ = do
  dip $ forcedCoerce_ @a @("issuer" :! a)
  unTransferParams
  forcedCoerce_ @(a, [a]) @("from" :! a, "tos" :! [a])
  stackType @(("from" :! a, "tos" :! [a]) & "issuer" :! a & [[a]] & Users a & Filterlists & s)
  unpair
  stackType @("from" :! a & "tos" :! [a] & "issuer" :! a & [[a]] & Users a & Filterlists & s)
  dig @2
  dup
  stackType @("issuer" :! a & "issuer" :! a & "from" :! a & "tos" :! [a] & [[a]] & Users a & Filterlists & s)
  dip $ do
    stackType @("issuer" :! a & "from" :! a & "tos" :! [a] & [[a]] & Users a & Filterlists & s)
    forcedCoerce_ @("issuer" :! a) @a
    dip $ do
      dup
      forcedCoerce_ @("from" :! a) @a
    stackType @(a & a & "from" :! a & "tos" :! [a] & [[a]] & Users a & Filterlists & s)
    ifEq
      (do
        drop
        forcedCoerce_ @("tos" :! [a]) @[a]
        cons
      )
      (do
        stackType @("from" :! a & "tos" :! [a] & [[a]] & Users a & Filterlists & s)
        dig @3
        dup
        stackType @(Users a & Users a & "from" :! a & "tos" :! [a] & [[a]] & Filterlists & s)
        dip $ do
          stackType @(Users a & "from" :! a & "tos" :! [a] & [[a]] & Filterlists & s)
          swap
          forcedCoerce_ @("from" :! a) @a
          assertUserFilterlist
          forcedCoerce_ @FilterlistId @("from" :! FilterlistId)
          stackType @("from" :! FilterlistId & "tos" :! [a] & [[a]] & Filterlists & s)
          dig @3
          dup
          stackType @(Filterlists & Filterlists & "from" :! FilterlistId & "tos" :! [a] & [[a]] & s)
          dip $ do
            stackType @(Filterlists & "from" :! FilterlistId & "tos" :! [a] & [[a]] & s)
            swap
            forcedCoerce_ @("from" :! FilterlistId) @FilterlistId
            assertOutboundFilterlists
            assertUnrestrictedOutboundFilterlists
            forcedCoerce_ @(Set FilterlistId) @("from" :! Set FilterlistId)
            stackType @("from" :! Set FilterlistId & "tos" :! [a] & [[a]] & s)
        stackType @(Users a & Filterlists & "from" :! Set FilterlistId & "tos" :! [a] & [[a]] & s)
        dup
        dip $ do
          stackType @(Users a & Filterlists & "from" :! Set FilterlistId & "tos" :! [a] & [[a]] & s)
          swap
          dug @3
          stackType @(Users a & "from" :! Set FilterlistId & "tos" :! [a] & Filterlists & [[a]] & s)
          dig @2
          forcedCoerce_ @("tos" :! [a]) @[a]
          iter $ do
            stackType @(a & Users a & "from" :! Set FilterlistId & Filterlists & [[a]] & s)
            swap
            dup
            dip $ do
              swap
              stackType @(a & Users a & "from" :! Set FilterlistId & Filterlists & [[a]] & s)
              assertUserFilterlist
              dip $ do
                dup
                forcedCoerce_ @("from" :! Set FilterlistId) @(Set FilterlistId)
              mem
              assert $ mkMTextUnsafe "outbound not filterlisted"
          dropN @2
        dig @2
        stackType @([[a]] & Users a & Filterlists & s)
      )
  forcedCoerce_ @("issuer" :! a) @a

-- | `assertTransfer` for a list of `TransferParams`
assertTransfer ::
     forall a s. (NiceComparable a, IsComparable a)
  => TransferParams a & Storage a & s :-> [[a]] & [Operation] & Storage a & s
assertTransfer = do
  dip $ do
    dup
    unStorage
    unpair
    dip $ do
      car
      dip $ do
        nil @Operation
    unpair
    dip $ nil @[a]
  assertTransfer_ @a
  swap
  dip $ do
    dropN @3
    -- pair


-- | `assertTransfer` for a list of `TransferParams`
assertTransfers ::
     forall a s. (NiceComparable a, IsComparable a)
  => [TransferParams a] & Storage a & s :-> [[a]] & [Operation] & Storage a & s
assertTransfers = do
  dip $ do
    dup
    unStorage
    unpair
    dip $ do
      car
      dip $ do
        nil @Operation
    unpair
    dip $ nil @[a]
  iter (assertTransfer_ @a)
  swap
  dip $ do
    dropN @3
    -- pair

-- | Assert that all users are filterlisted and `unrestricted`, or the issuer
assertReceivers ::
     forall a s. (NiceComparable a, IsComparable a)
  => [a] & [Operation] & Storage a & s :-> [Operation] & Storage a & s
assertReceivers = do
  dip $ do
    dip $ do
      dup
      unStorage
      unpair
      dip car
      unpair
      stackType @(a & Users a & Filterlists & Storage a & s)
  iter $ do
    swap
    dip assertReceiver
  dip $ dropN @3
  -- nil
  -- pair

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

