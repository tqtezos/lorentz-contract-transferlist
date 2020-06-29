{-# LANGUAGE RebindableSyntax #-}

module Lorentz.Contracts.Filterlist.Wrapper.ManagedLedger where

import Prelude hiding ((>>), drop, swap, get, some)

import Lorentz

import qualified Lorentz.Contracts.Filterlist.Types as Filterlist
import qualified Lorentz.Contracts.Filterlist.Wrapper as Wrapper

import qualified Lorentz.Contracts.ManagedLedger as ManagedLedger

-- | `ManagedLedger.managedLedgerContract` with filterlisting for transfers
filterlistedManagedLedgerContract ::
  ContractCode
    (Wrapper.Parameter ManagedLedger.Parameter Address)
    (Wrapper.Storage ManagedLedger.Storage Address)
filterlistedManagedLedgerContract =
  Wrapper.filterlistWrappedContract
    managedLedgerTransferParams
    ManagedLedger.managedLedgerContract

-- | If `ManagedLedger.Parameter` is `ManagedLedger.Transfer`,
-- emit `Just` the to/from addresses, otherwise emit `Nothing`
managedLedgerTransferParams :: forall s. ()
  => ManagedLedger.Parameter & s :-> Maybe (Filterlist.TransferParams Address) & s
managedLedgerTransferParams = do
  caseT @ManagedLedger.Parameter
    ( #cTransfer /-> do
        forcedCoerce_ @("from" :! Address, "to" :! Address, "value" :! Natural) @(Address, (Address, Natural))
        unpair
        dip car
        pair
        Filterlist.toTransferParams
        some
    , #cApprove /-> drop >> none
    , #cApproveCAS /-> drop >> none
    , #cGetAllowance /-> drop >> none
    , #cGetBalance /-> drop >> none
    , #cGetTotalSupply /-> drop >> none
    , #cSetPause /-> drop >> none
    , #cSetAdministrator /-> drop >> none
    , #cGetAdministrator /-> drop >> none
    , #cMint /-> drop >> none
    , #cBurn /-> drop >> none
    )

