module Test.Transferlist where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Test.Hspec (Expectation)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Lorentz
import Lorentz.Test

import qualified Lorentz.Contracts.Transferlist as Transferlist
import qualified Lorentz.Contracts.Transferlist.Types as Transferlist

shouldSucceed :: Bool
shouldSucceed = True

shouldFail :: Bool
shouldFail = False

withTransferlistContract :: ()
  => Address
  -> [(Address, Transferlist.TransferlistId)]
  -> [(Transferlist.TransferlistId, (Bool, [Transferlist.TransferlistId]))]
  -> Address
  -> (TAddress (Transferlist.Parameter Address) -> IntegrationalScenario)
  -> Expectation
withTransferlistContract issuer' users' transferlists' admin' callback =
  integrationalTestExpectation $ do
    transferlistContract' <-
      lOriginate
        (Transferlist.transferlistContract @Address)
        "Transferlist Contract"
        (Transferlist.Storage
           issuer'
           (Transferlist.mkUsers users')
           (Transferlist.mkTransferlists transferlists')
           admin')
        (toMutez 0)
    callback transferlistContract'

assertTransfer :: ()
  => String
  -> Bool
  -> Address
  -> [(Address, Transferlist.TransferlistId)]
  -> [(Transferlist.TransferlistId, (Bool, [Transferlist.TransferlistId]))]
  -> Address
  -> Address
  -> Address
  -> TestTree
assertTransfer description' shouldSucceed' issuer' users' transferlists' admin' from' to' =
  testCase description' $
  withTransferlistContract
    issuer'
    users'
    transferlists'
    admin' $ \transferlistContract' -> do
      lCall transferlistContract' . Transferlist.AssertTransfers $ [Transferlist.TransferParams from' [to']]
      if shouldSucceed'
         then validate . Right $
           expectAnySuccess
         else validate . Left $
           lExpectMichelsonFailed (const True) transferlistContract'

test_AssertTransfer :: TestTree
test_AssertTransfer = testGroup "AssertTransfer"
  [ assertTransfer "Assert transfer with no users" shouldFail
      genesisAddress1 mempty mempty genesisAddress2 genesisAddress3 genesisAddress4
  , assertTransfer "Assert transfer from issuer with no users" shouldFail
      genesisAddress1 mempty mempty genesisAddress2 genesisAddress1 genesisAddress1
  , assertTransfer "Assert transfer from admin with no users" shouldFail
      genesisAddress1 mempty mempty genesisAddress2 genesisAddress2 genesisAddress2
  , assertTransfer "Assert transfer from issuer to a user" shouldSucceed
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, []))] genesisAddress2 genesisAddress1 genesisAddress3
  , assertTransfer "Assert transfer from transferlisted user to self (not on own transferlist)" shouldFail
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, []))] genesisAddress2 genesisAddress3 genesisAddress3
  , assertTransfer "Assert transfer from transferlisted user to self (on own transferlist)" shouldSucceed
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, [0]))] genesisAddress2 genesisAddress3 genesisAddress3
  , assertTransfer "Assert transfer from transferlisted user to self (on own transferlist, restricted)" shouldFail
      genesisAddress1 [(genesisAddress3, 0)] [(0, (False, [0]))] genesisAddress2 genesisAddress3 genesisAddress3
  , assertTransfer "Assert transfer from transferlisted user to other user on same transferlist (no outbound)" shouldFail
      genesisAddress1 [(genesisAddress3, 0), (genesisAddress4, 0)] [(0, (True, []))] genesisAddress2 genesisAddress3 genesisAddress4
  , assertTransfer "Assert transfer from transferlisted user to other user on same transferlist (with outbound)" shouldSucceed
      genesisAddress1 [(genesisAddress3, 0), (genesisAddress4, 0)] [(0, (True, [0]))] genesisAddress2 genesisAddress3 genesisAddress4
  , assertTransfer "Assert transfer from transferlisted user to other user on other transferlist (no outbound)" shouldFail
      genesisAddress1 [(genesisAddress3, 0), (genesisAddress4, 1)] [(0, (True, [])), (1, (True, []))] genesisAddress2 genesisAddress3 genesisAddress4
  , assertTransfer "Assert transfer from transferlisted user to other user on other transferlist (with outbound)" shouldSucceed
      genesisAddress1 [(genesisAddress3, 0), (genesisAddress4, 1)] [(0, (True, [1])), (1, (True, []))] genesisAddress2 genesisAddress3 genesisAddress4
  ]

assertTransfers :: ()
  => String
  -> Bool
  -> Address
  -> [(Address, Transferlist.TransferlistId)]
  -> [(Transferlist.TransferlistId, (Bool, [Transferlist.TransferlistId]))]
  -> Address
  -> [(Address, [Address])]
  -> TestTree
assertTransfers description' shouldSucceed' issuer' users' transferlists' admin' transfers' =
  testCase description' $
  withTransferlistContract
    issuer'
    users'
    transferlists'
    admin' $ \transferlistContract' -> do
      lCall transferlistContract' $ Transferlist.AssertTransfers $ uncurry Transferlist.TransferParams <$> transfers'
      if shouldSucceed'
         then validate . Right $
           expectAnySuccess
         else validate . Left $
           lExpectMichelsonFailed (const True) transferlistContract'

test_AssertTransfers :: TestTree
test_AssertTransfers = testGroup "AssertTransfers"
  [ assertTransfers "Assert transfer with no users" shouldFail
      genesisAddress1 mempty mempty genesisAddress2 [(genesisAddress3, [genesisAddress4])]
  , assertTransfers "Assert transfer from issuer with no users" shouldFail
      genesisAddress1 mempty mempty genesisAddress2 [(genesisAddress1, [genesisAddress1])]
  , assertTransfers "Assert transfer from issuer with no users (2x)" shouldFail
      genesisAddress1 mempty mempty genesisAddress2 $ replicate 2 (genesisAddress1, [genesisAddress1])
  , assertTransfers "Assert transfer from issuer with no users (3x)" shouldFail
      genesisAddress1 mempty mempty genesisAddress2 $ replicate 3 (genesisAddress1, [genesisAddress1])
  , assertTransfers "Assert transfer from admin with no users" shouldFail
      genesisAddress1 mempty mempty genesisAddress2 [(genesisAddress2, [genesisAddress2])]
  , assertTransfers "Assert transfer from issuer to a user" shouldSucceed
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, []))] genesisAddress2 [(genesisAddress1, [genesisAddress3])]
  , assertTransfers "Assert transfer from issuer to a user (2x)" shouldSucceed
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, []))] genesisAddress2 $ replicate 2 (genesisAddress1, [genesisAddress3])
  , assertTransfers "Assert transfer from issuer to a user (3x)" shouldSucceed
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, []))] genesisAddress2 $ replicate 3 (genesisAddress1, [genesisAddress3])
  , assertTransfers "Assert transfer from issuer to a user (3x), then from non-user" shouldFail
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, []))] genesisAddress2 $ replicate 3 (genesisAddress1, [genesisAddress3]) ++ [(genesisAddress4, [genesisAddress4])]
  , assertTransfers "Assert transfer from issuer to a user (2x), batched" shouldSucceed
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, []))] genesisAddress2 $ [(genesisAddress1, replicate 2 genesisAddress3)]
  , assertTransfers "Assert transfer from issuer to a user (3x)" shouldSucceed
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, []))] genesisAddress2 $ [(genesisAddress1, replicate 3 genesisAddress3)]
  , assertTransfers "Assert transfer from issuer to a user (3x), then to non-user" shouldFail
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, []))] genesisAddress2 $ [(genesisAddress1, replicate 3 genesisAddress3 ++ [genesisAddress4])]
  , assertTransfers "Assert transfer from transferlisted user to self (not on own transferlist)" shouldFail
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, []))] genesisAddress2 [(genesisAddress3, [genesisAddress3])]
  , assertTransfers "Assert transfer from transferlisted user to self (on own transferlist)" shouldSucceed
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, [0]))] genesisAddress2 [(genesisAddress3, [genesisAddress3])]
  , assertTransfers "Assert transfer from transferlisted user to self (on own transferlist) (2x)" shouldSucceed
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, [0]))] genesisAddress2 [(genesisAddress3, replicate 2 genesisAddress3)]
  , assertTransfers "Assert transfer from transferlisted user to self (on own transferlist) (3x)" shouldSucceed
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, [0]))] genesisAddress2 [(genesisAddress3, replicate 3 genesisAddress3)]
  , assertTransfers "Assert transfer from transferlisted user to self (on own transferlist) (2x)" shouldSucceed
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, [0]))] genesisAddress2 $ replicate 2 (genesisAddress3, [genesisAddress3])
  , assertTransfers "Assert transfer from transferlisted user to self (on own transferlist) (3x)" shouldSucceed
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, [0]))] genesisAddress2 $ replicate 3 (genesisAddress3, [genesisAddress3])
  , assertTransfers "Assert transfer from transferlisted user to self (on own transferlist) (3x), then to non-user" shouldFail
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, [0]))] genesisAddress2 $ replicate 3 (genesisAddress3, [genesisAddress3]) ++ [(genesisAddress3, [genesisAddress4])]
  , assertTransfers "Assert transfer from transferlisted user to self (on own transferlist) (3x), then to self and non-user" shouldFail
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, [0]))] genesisAddress2 $ replicate 3 (genesisAddress3, [genesisAddress3]) ++ [(genesisAddress3, [genesisAddress3, genesisAddress4])]
  , assertTransfers "Assert transfer from transferlisted user to self (on own transferlist, restricted)" shouldFail
      genesisAddress1 [(genesisAddress3, 0)] [(0, (False, [0]))] genesisAddress2 [(genesisAddress3, [genesisAddress3])]
  ]

test_AssertReceivers :: TestTree
test_AssertReceivers = testGroup "AssertReceivers"
  [ testCase "Assert receivers with no users" $ do
      withTransferlistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \transferlistContract' -> do
          lCall transferlistContract' $ Transferlist.AssertReceivers . return $ genesisAddress3
          validate . Left $
            lExpectMichelsonFailed (const True) transferlistContract'
  , testCase "Assert receivers issuer with no users" $ do
      withTransferlistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \transferlistContract' -> do
          lCall transferlistContract' $ Transferlist.AssertReceivers . return $ genesisAddress1
          validate . Left $
            lExpectMichelsonFailed (const True) transferlistContract'
  , testCase "Assert receivers admin with no users" $ do
      withTransferlistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \transferlistContract' -> do
          lCall transferlistContract' $ Transferlist.AssertReceivers . return $ genesisAddress2
          validate . Left $
            lExpectMichelsonFailed (const True) transferlistContract'
  , testCase "Assert receivers issuer with user" $ do
      withTransferlistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        [(0, (True, []))]
        genesisAddress2 $ \transferlistContract' -> do
          lCall transferlistContract' $ Transferlist.AssertReceivers . return $ genesisAddress1
          validate . Left $
            lExpectMichelsonFailed (const True) transferlistContract'
  , testCase "Assert receivers on transferlisted user (not on own transferlist)" $ do
      withTransferlistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        [(0, (True, []))]
        genesisAddress2 $ \transferlistContract' -> do
          lCall transferlistContract' $ Transferlist.AssertReceivers . return $ genesisAddress3
          validate . Right $ expectAnySuccess
  , testCase "Assert receivers on transferlisted user (on own transferlist)" $ do
      withTransferlistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        [(0, (True, [0]))]
        genesisAddress2 $ \transferlistContract' -> do
          lCall transferlistContract' $ Transferlist.AssertReceivers . return $ genesisAddress3
          validate . Right $ expectAnySuccess
  , testCase "Assert receivers on transferlisted user (on own transferlist, restricted)" $ do
      withTransferlistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        [(0, (False, [0]))]
        genesisAddress2 $ \transferlistContract' -> do
          lCall transferlistContract' $ Transferlist.AssertReceivers . return $ genesisAddress3
          validate . Left $
            lExpectMichelsonFailed (const True) transferlistContract'
  , testCase "Assert receivers with empty list" $ do
      withTransferlistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \transferlistContract' -> do
          lCall transferlistContract' $ Transferlist.AssertReceivers []
          validate . Right $ expectAnySuccess
  , testCase "Assert receivers with multiple valid" $ do
      withTransferlistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        [(0, (True, []))]
        genesisAddress2 $ \transferlistContract' -> do
          lCall transferlistContract' $ Transferlist.AssertReceivers $ replicate 10 genesisAddress3
          validate . Right $ expectAnySuccess
  , testCase "Assert receivers with multiple valid and one invalid" $ do
      withTransferlistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        [(0, (True, []))]
        genesisAddress2 $ \transferlistContract' -> do
          lCall transferlistContract' $ Transferlist.AssertReceivers $ replicate 10 genesisAddress3 ++ [genesisAddress4]
          validate . Left $
            lExpectMichelsonFailed (const True) transferlistContract'
  ]

test_SetIssuer :: TestTree
test_SetIssuer = testGroup "SetIssuer"
  [ testCase "Set issuer as non-admin" $ do
      withTransferlistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \transferlistContract' -> do
          lCall transferlistContract' . Transferlist.OtherParameter $ Transferlist.SetIssuer genesisAddress3
          validate . Left $
            lExpectMichelsonFailed (const True) transferlistContract'
  , testCase "Set issuer as admin" $ do
      withTransferlistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress $ \transferlistContract' -> do
          lCall transferlistContract' . Transferlist.OtherParameter $ Transferlist.SetIssuer genesisAddress3
          validate . Right $ lExpectStorageUpdate @(Transferlist.Storage Address) transferlistContract' $ \st ->
            if Transferlist.issuer st == genesisAddress3
               then return ()
               else Left $ CustomValidationError "Issuer was not updated"
  ]


test_SetAdmin :: TestTree
test_SetAdmin = testGroup "SetAdmin"
  [ testCase "Set admin as non-admin" $ do
      withTransferlistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \transferlistContract' -> do
          lCall transferlistContract' . Transferlist.OtherParameter $ Transferlist.SetAdmin genesisAddress3
          validate . Left $
            lExpectMichelsonFailed (const True) transferlistContract'
  , testCase "Set admin as admin" $ do
      withTransferlistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress $ \transferlistContract' -> do
          lCall transferlistContract' . Transferlist.OtherParameter $ Transferlist.SetAdmin genesisAddress3
          validate . Right $ lExpectStorageUpdate @(Transferlist.Storage Address) transferlistContract' $ \st ->
            if Transferlist.admin st == genesisAddress3
               then return ()
               else Left $ CustomValidationError "Admin was not updated"
  ]

test_UpdateUser :: TestTree
test_UpdateUser = testGroup "UpdateUser"
  [ testCase "Update user as non-admin" $ do
      withTransferlistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \transferlistContract' -> do
          lCall transferlistContract' . Transferlist.OtherParameter $ Transferlist.UpdateUser $ Transferlist.UpdateUserParams genesisAddress3 Nothing
          validate . Left $
            lExpectMichelsonFailed (const True) transferlistContract'
  , testCase "Update user as admin" $ do
      withTransferlistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress $ \transferlistContract' -> do
          lCall transferlistContract' . Transferlist.OtherParameter $ Transferlist.UpdateUser $ Transferlist.UpdateUserParams genesisAddress3 Nothing
          validate . Right $ expectAnySuccess
  , testCase "Add user as admin" $ do
      withTransferlistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress $ \transferlistContract' -> do
          lCall transferlistContract' . Transferlist.OtherParameter $ Transferlist.UpdateUser $ Transferlist.UpdateUserParams genesisAddress3 (Just 0)
          validate . Right $ lExpectStorageUpdate @(Transferlist.Storage Address) transferlistContract' $ \st ->
            if Map.assocs (unBigMap $ Transferlist.users st) == [(genesisAddress3, 0)]
               then return ()
               else Left $ CustomValidationError "User was not added"
  , testCase "Add existing user as admin" $ do
      withTransferlistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        mempty
        genesisAddress $ \transferlistContract' -> do
          lCall transferlistContract' . Transferlist.OtherParameter $ Transferlist.UpdateUser $ Transferlist.UpdateUserParams genesisAddress3 (Just 0)
          validate . Right $ expectNoStorageUpdates
  , testCase "Update existing user as admin" $ do
      withTransferlistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        mempty
        genesisAddress $ \transferlistContract' -> do
          lCall transferlistContract' . Transferlist.OtherParameter $ Transferlist.UpdateUser $ Transferlist.UpdateUserParams genesisAddress3 (Just 1)
          validate . Right $ lExpectStorageUpdate @(Transferlist.Storage Address) transferlistContract' $ \st ->
            if Map.assocs (unBigMap $ Transferlist.users st) == [(genesisAddress3, 1)]
               then return ()
               else Left $ CustomValidationError "User was not updated"
  , testCase "Remove user as admin" $ do
      withTransferlistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        mempty
        genesisAddress $ \transferlistContract' -> do
          lCall transferlistContract' . Transferlist.OtherParameter $ Transferlist.UpdateUser $ Transferlist.UpdateUserParams genesisAddress3 Nothing
          validate . Right $ lExpectStorageUpdate @(Transferlist.Storage Address) transferlistContract' $ \st ->
            if Transferlist.users st == mempty
               then return ()
               else Left $ CustomValidationError "User was not removed"
  ]

test_SetTransferlistOutbound :: TestTree
test_SetTransferlistOutbound = testGroup "SetTransferlistOutbound"
  [ testCase "Update outbound transferlists as non-admin" $ do
      withTransferlistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \transferlistContract' -> do
          lCall transferlistContract' . Transferlist.OtherParameter $ Transferlist.SetTransferlistOutbound $ Transferlist.TransferlistOutboundParams 0 Nothing
          validate . Left $
            lExpectMichelsonFailed (const True) transferlistContract'
  , testCase "Update outbound transferlists as admin" $ do
      withTransferlistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress $ \transferlistContract' -> do
          lCall transferlistContract' . Transferlist.OtherParameter $ Transferlist.SetTransferlistOutbound $ Transferlist.TransferlistOutboundParams 0 Nothing
          validate . Right $ expectAnySuccess
  , testCase "Add outbound transferlists as admin" $ do
      withTransferlistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress $ \transferlistContract' -> do
          lCall transferlistContract' . Transferlist.OtherParameter $ Transferlist.SetTransferlistOutbound $ Transferlist.TransferlistOutboundParams 0 $ Just $ Transferlist.OutboundTransferlists True mempty
          validate . Right $ lExpectStorageUpdate @(Transferlist.Storage Address) transferlistContract' $ \st ->
            if Map.assocs (unBigMap $ Transferlist.transferlists st) == [(0, Transferlist.OutboundTransferlists True mempty)]
               then return ()
               else Left $ CustomValidationError "User was not added"
  , testCase "Add existing outbound transferlists as admin" $ do
      withTransferlistContract
        genesisAddress1
        mempty
        [(0, (True, []))]
        genesisAddress $ \transferlistContract' -> do
          lCall transferlistContract' . Transferlist.OtherParameter $ Transferlist.SetTransferlistOutbound $ Transferlist.TransferlistOutboundParams 0 $ Just $ Transferlist.OutboundTransferlists True mempty
          validate . Right $ expectNoStorageUpdates
  , testCase "Update existing outbound transferlists as admin" $ do
      withTransferlistContract
        genesisAddress1
        mempty
        [(0, (True, []))]
        genesisAddress $ \transferlistContract' -> do
          lCall transferlistContract' . Transferlist.OtherParameter $ Transferlist.SetTransferlistOutbound $ Transferlist.TransferlistOutboundParams 0 $ Just $ Transferlist.OutboundTransferlists False (Set.singleton 0)
          validate . Right $ lExpectStorageUpdate @(Transferlist.Storage Address) transferlistContract' $ \st ->
            if Map.assocs (unBigMap $ Transferlist.transferlists st) == [(0, Transferlist.OutboundTransferlists False (Set.singleton 0))]
               then return ()
               else Left $ CustomValidationError "User was not updated"
  , testCase "Remove outbound transferlist as admin" $ do
      withTransferlistContract
        genesisAddress1
        mempty
        [(0, (True, []))]
        genesisAddress $ \transferlistContract' -> do
          lCall transferlistContract' . Transferlist.OtherParameter $ Transferlist.SetTransferlistOutbound $ Transferlist.TransferlistOutboundParams 0 Nothing
          validate . Right $ lExpectStorageUpdate @(Transferlist.Storage Address) transferlistContract' $ \st ->
            if Transferlist.transferlists st == mempty
               then return ()
               else Left $ CustomValidationError "User was not removed"
  ]



