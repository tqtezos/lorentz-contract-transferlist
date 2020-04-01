{-# LANGUAGE RebindableSyntax #-}

module Lorentz.Contracts.Whitelist.Wrapper.ManagedLedger where

import Prelude hiding ((>>), drop, swap, get, some)

import Lorentz

import Lorentz.Contracts.Util ()
import qualified Lorentz.Contracts.Whitelist.Types as Whitelist
import qualified Lorentz.Contracts.Whitelist.Wrapper as Wrapper

import qualified Lorentz.Contracts.ManagedLedger as ManagedLedger

-- | `ManagedLedger.managedLedgerContract` with whitelisting for transfers
whitelistedManagedLedgerContract ::
  Contract
    (Wrapper.Parameter ManagedLedger.Parameter Address)
    (Wrapper.Storage ManagedLedger.Storage Address)
whitelistedManagedLedgerContract =
  Wrapper.whitelistWrappedContract
    managedLedgerTransferParams
    ManagedLedger.managedLedgerContract

-- | If `ManagedLedger.Parameter` is `ManagedLedger.Transfer`,
-- emit `Just` the to/from addresses, otherwise emit `Nothing`
managedLedgerTransferParams :: forall s. ()
  => ManagedLedger.Parameter & s :-> Maybe (Whitelist.TransferParams Address) & s
managedLedgerTransferParams = do
  caseT @ManagedLedger.Parameter
    ( #cTransfer /-> do
        coerce_ @("from" :! Address, "to" :! Address, "value" :! Natural) @(Address, (Address, Natural))
        unpair
        dip car
        pair
        Whitelist.toTransferParams
        some
    , #cApprove /-> drop >> none
    , #cGetAllowance /-> drop >> none
    , #cGetBalance /-> drop >> none
    , #cGetTotalSupply /-> drop >> none
    , #cSetPause /-> drop >> none
    , #cSetAdministrator /-> drop >> none
    , #cGetAdministrator /-> drop >> none
    , #cMint /-> drop >> none
    , #cBurn /-> drop >> none
    )

