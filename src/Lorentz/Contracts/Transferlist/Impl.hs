{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS -Wno-unused-do-bind #-}

module Lorentz.Contracts.Transferlist.Impl where

import Prelude hiding ((>>), drop, swap, get)

import Lorentz
import Michelson.Text
import Michelson.Typed.Haskell.Value (IsComparable)

import Lorentz.Contracts.Transferlist.Types


--------------
-- Entrypoints
--------------

-- | Assert that one user is allowed to transfer to the other,
-- preserving the storage for additional calls.
--
-- The `issuer` is allowed to transfer to anyone.
--
-- If the sender's `TransferlistId`'s `OutboundTransferlists` is `unrestricted`,
-- they may transfer to any receiver whose `TransferlistId` is in their
-- `allowedTransferlists`.
assertTransfer_ ::
     forall a s. (NiceComparable a, IsComparable a)
  => TransferParams a & a & [[a]] & Users a & Transferlists & s :-> a & [[a]] & Users a & Transferlists & s
assertTransfer_ = do
  dip $ forcedCoerce_ @a @("issuer" :! a)
  unTransferParams
  forcedCoerce_ @(a, [a]) @("from" :! a, "tos" :! [a])
  stackType @(("from" :! a, "tos" :! [a]) & "issuer" :! a & [[a]] & Users a & Transferlists & s)
  unpair
  stackType @("from" :! a & "tos" :! [a] & "issuer" :! a & [[a]] & Users a & Transferlists & s)
  dig @2
  dup
  stackType @("issuer" :! a & "issuer" :! a & "from" :! a & "tos" :! [a] & [[a]] & Users a & Transferlists & s)
  dip $ do
    stackType @("issuer" :! a & "from" :! a & "tos" :! [a] & [[a]] & Users a & Transferlists & s)
    forcedCoerce_ @("issuer" :! a) @a
    dip $ do
      dup
      forcedCoerce_ @("from" :! a) @a
    stackType @(a & a & "from" :! a & "tos" :! [a] & [[a]] & Users a & Transferlists & s)
    ifEq
      (do
        drop
        forcedCoerce_ @("tos" :! [a]) @[a]
        cons
      )
      (do
        stackType @("from" :! a & "tos" :! [a] & [[a]] & Users a & Transferlists & s)
        dig @3
        dup
        stackType @(Users a & Users a & "from" :! a & "tos" :! [a] & [[a]] & Transferlists & s)
        dip $ do
          stackType @(Users a & "from" :! a & "tos" :! [a] & [[a]] & Transferlists & s)
          swap
          forcedCoerce_ @("from" :! a) @a
          assertUserTransferlist
          forcedCoerce_ @TransferlistId @("from" :! TransferlistId)
          stackType @("from" :! TransferlistId & "tos" :! [a] & [[a]] & Transferlists & s)
          dig @3
          dup
          stackType @(Transferlists & Transferlists & "from" :! TransferlistId & "tos" :! [a] & [[a]] & s)
          dip $ do
            stackType @(Transferlists & "from" :! TransferlistId & "tos" :! [a] & [[a]] & s)
            swap
            forcedCoerce_ @("from" :! TransferlistId) @TransferlistId
            assertOutboundTransferlists
            assertUnrestrictedOutboundTransferlists
            forcedCoerce_ @(Set TransferlistId) @("from" :! Set TransferlistId)
            stackType @("from" :! Set TransferlistId & "tos" :! [a] & [[a]] & s)
        stackType @(Users a & Transferlists & "from" :! Set TransferlistId & "tos" :! [a] & [[a]] & s)
        dup
        dip $ do
          stackType @(Users a & Transferlists & "from" :! Set TransferlistId & "tos" :! [a] & [[a]] & s)
          swap
          dug @3
          stackType @(Users a & "from" :! Set TransferlistId & "tos" :! [a] & Transferlists & [[a]] & s)
          dig @2
          forcedCoerce_ @("tos" :! [a]) @[a]
          iter $ do
            stackType @(a & Users a & "from" :! Set TransferlistId & Transferlists & [[a]] & s)
            swap
            dup
            dip $ do
              swap
              stackType @(a & Users a & "from" :! Set TransferlistId & Transferlists & [[a]] & s)
              assertUserTransferlist
              dip $ do
                dup
                forcedCoerce_ @("from" :! Set TransferlistId) @(Set TransferlistId)
              mem
              assert $ mkMTextUnsafe "outbound not transferlisted"
          dropN @2
        dig @2
        stackType @([[a]] & Users a & Transferlists & s)
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
  dip $ dropN @3

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

-- | Assert that all users are transferlisted and `unrestricted`, or the issuer
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
      stackType @(a & Users a & Transferlists & Storage a & s)
  iter $ do
    swap
    dip assertReceiver
  dip $ dropN @3

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
      -- transferlists & admin
      stackType @(Transferlists & Address & '[])
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

-- | Add/update a user with a particular `TransferlistId`,
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
      -- transferlists & admin
      stackType @(Transferlists & Address & '[])
      dip $ do
        assertAdmin
      pair
    unpair
  unUpdateUserParams
  -- user & new_transferlist & issuer & users & cdr store
  stackType @(a & Maybe TransferlistId & a & Users a & (Transferlists, Address) & '[])
  swap
  dip assertNotIssuer
  pair
  swap
  dip $ do
    unpair
    swap
    updateUserTransferlist
  pair
  pair
  toStorage
  nil
  pair

-- | Set the `TransferlistOutboundParams` for a `TransferlistId`
--
-- Only admin
setTransferlistOutbound :: forall a. () => Entrypoint TransferlistOutboundParams (Storage a)
setTransferlistOutbound = do
  dip $ do
    unStorage
    unpair
    swap
    unpair
    dip assertAdmin
  unTransferlistOutboundParams
  setOutboundTransferlists
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

-- | Get a user's `TransferlistId`, or `Nothing` if the user is not present
getUser :: forall a. (IsComparable a) => Entrypoint (View a (Maybe TransferlistId)) (Storage a)
getUser =
  view_ $ do
    unpair
    dip $ do
      unStorage
      car
      cdr
    getUserTransferlist

assertTransferlist :: forall a. () => Entrypoint AssertTransferlistParams (Storage a)
assertTransferlist = do
  unAssertTransferlistParams
  dip $ do
    dup
    unStorage
    cdr
    car
  unpair
  swap
  dip $ do
    outboundTransferlists
  ifNone
    (assertNone $ mkMTextUnsafe "exists")
    (do
      dip . assertSome $ mkMTextUnsafe "doesn't exist"
      assertSubsetOutboundTransferlists
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

