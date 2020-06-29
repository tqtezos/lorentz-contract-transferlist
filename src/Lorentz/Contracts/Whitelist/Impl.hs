{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS -Wno-unused-do-bind #-}

module Lorentz.Contracts.Whitelist.Impl where

import Prelude hiding ((>>), drop, swap, get)

import Lorentz
import Michelson.Text
import Michelson.Typed.Haskell.Value (IsComparable)

import Lorentz.Contracts.Whitelist.Types


--------------
-- Entrypoints
--------------


-- | Assert that one user is allowed to transfer to the other,
-- preserving the storage for additional calls.
--
-- The `issuer` is allowed to transfer to anyone.
--
-- If the sender's `WhitelistId`'s `OutboundWhitelists` is `unrestricted`,
-- they may transfer to any receiver whose `WhitelistId` is in their
-- `allowedWhitelists`.
assertTransfer_ ::
     forall a s. (NiceComparable a, IsComparable a)
  => TransferParams a & a & Users a & Whitelists & s :-> a & Users a & Whitelists & s
assertTransfer_ = do
  unTransferParams
  dip $ forcedCoerce_ @a @("issuer" :! a)
  forcedCoerce_ @(a, a) @("from" :! a, "to" :! a)
  swap
  stackType @("issuer" :! a & ("from" :! a, "to" :! a) & Users a & Whitelists & s)
  dip $ do
    dup
    dip $ do
      dip dup
      cdr
      forcedCoerce_ @("to" :! a) @a
      assertUserWhitelist
      forcedCoerce_ @WhitelistId @("to" :! WhitelistId)
    dup
    car
  stackType @("issuer" :! a & "from" :! a & ("from" :! a, "to" :! a) & "to" :! WhitelistId & Users a & Whitelists & s)
  dup
  dip $ do
    stackType @("issuer" :! a & "from" :! a & ("from" :! a, "to" :! a) & "to" :! WhitelistId & Users a & Whitelists & s)
    -- dipN @3 $ do
    dipN @4 $ do
      pair
      dup
      dip unpair
      unpair
    stackType @("issuer" :! a & "from" :! a & ("from" :! a, "to" :! a) & "to" :! WhitelistId & Users a & Whitelists & Users a & Whitelists & s)
    dip $ forcedCoerce_ @("from" :! a) @a
    forcedCoerce_ @("issuer" :! a) @a
    stackType @(a & a & ("from" :! a, "to" :! a) & "to" :! WhitelistId & Users a & Whitelists & Users a & Whitelists & s)
    ifEq -- 'from' user is issuer
      (do
        dropN @4
      )
      (do
        car
        dip $ do
          swap
        forcedCoerce_ @("from" :! a) @a
        assertUserWhitelist
        forcedCoerce_ @WhitelistId @("from" :! WhitelistId)
        swap
        dip $ do
          forcedCoerce_ @("from" :! WhitelistId) @WhitelistId
          assertOutboundWhitelists
          assertUnrestrictedOutboundWhitelists
        forcedCoerce_ @("to" :! WhitelistId) @WhitelistId
        mem
        stackType @(Bool & Users a & Whitelists & s)
        assert $ mkMTextUnsafe "outbound not whitelisted"
      )
    stackType @(Users a & Whitelists & s)
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
    -- issuer & users & whitelists & store
    stackType @(a & Users a & Whitelists & Storage a & s)
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

-- | Assert that all users are whitelisted and `unrestricted`, or the issuer
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
    stackType @(a & Users a & Whitelists & Storage a & s)
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
      -- whitelists & admin
      stackType @(Whitelists & Address & '[])
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

-- | Add/update a user with a particular `WhitelistId`,
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
      -- whitelists & admin
      stackType @(Whitelists & Address & '[])
      dip $ do
        assertAdmin
      pair
    unpair
  unUpdateUserParams
  -- user & new_whitelist & issuer & users & cdr store
  stackType @(a & Maybe WhitelistId & a & Users a & (Whitelists, Address) & '[])
  swap
  dip assertNotIssuer
  pair
  swap
  dip $ do
    unpair
    swap
    updateUserWhitelist
  pair
  pair
  toStorage
  nil
  pair

-- | Set the `WhitelistOutboundParams` for a `WhitelistId`
--
-- Only admin
setWhitelistOutbound :: forall a. () => Entrypoint WhitelistOutboundParams (Storage a)
setWhitelistOutbound = do
  dip $ do
    unStorage
    unpair
    swap
    unpair
    dip assertAdmin
  unWhitelistOutboundParams
  setOutboundWhitelists
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

-- | Get a user's `WhitelistId`, or `Nothing` if the user is not present
getUser :: forall a. (IsComparable a) => Entrypoint (View a (Maybe WhitelistId)) (Storage a)
getUser =
  view_ $ do
    unpair
    dip $ do
      unStorage
      car
      cdr
    getUserWhitelist

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
    outboundWhitelists
  ifNone
    (assertNone $ mkMTextUnsafe "exists")
    (do
      dip . assertSome $ mkMTextUnsafe "doesn't exist"
      assertSubsetOutboundWhitelists
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

