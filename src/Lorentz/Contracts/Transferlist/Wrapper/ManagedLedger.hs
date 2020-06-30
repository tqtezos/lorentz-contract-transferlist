{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS -Wno-unused-do-bind #-}

module Lorentz.Contracts.Transferlist.Wrapper.ManagedLedger where

import Prelude hiding ((>>), drop, swap, get, some)

import Lorentz

import qualified Lorentz.Contracts.Transferlist.Types as Transferlist
import qualified Lorentz.Contracts.Transferlist.Wrapper as Wrapper

import qualified Lorentz.Contracts.ManagedLedger as ManagedLedger

-- | `ManagedLedger.managedLedgerContract` with transferlisting for transfers
transferlistedManagedLedgerContract ::
  ContractCode
    (Wrapper.Parameter ManagedLedger.Parameter Address)
    (Wrapper.Storage ManagedLedger.Storage Address)
transferlistedManagedLedgerContract =
  Wrapper.transferlistWrappedContract
    managedLedgerTransferParams
    ManagedLedger.managedLedgerContract

-- | If `ManagedLedger.Parameter` is `ManagedLedger.Transfer`,
-- emit `Just` the to/from addresses, otherwise emit `Nothing`
managedLedgerTransferParams :: forall s. ()
  => ManagedLedger.Parameter & s :-> Maybe (Transferlist.TransferParams Address) & s
managedLedgerTransferParams = do
  caseT @ManagedLedger.Parameter
    ( #cTransfer /-> do
        forcedCoerce_ @("from" :! Address, "to" :! Address, "value" :! Natural) @(Address, (Address, Natural))
        unpair
        dip $ do
          car
          dip nil
          cons
        pair
        Transferlist.toTransferParams
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

