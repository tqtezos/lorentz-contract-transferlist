module Test.Filterlist where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Test.Hspec (Expectation)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Lorentz
import Lorentz.Test

import qualified Lorentz.Contracts.Filterlist as Filterlist
import qualified Lorentz.Contracts.Filterlist.Types as Filterlist

shouldSucceed :: Bool
shouldSucceed = True

shouldFail :: Bool
shouldFail = False

withFilterlistContract :: ()
  => Address
  -> [(Address, Filterlist.FilterlistId)]
  -> [(Filterlist.FilterlistId, (Bool, [Filterlist.FilterlistId]))]
  -> Address
  -> (TAddress (Filterlist.Parameter Address) -> IntegrationalScenario)
  -> Expectation
withFilterlistContract issuer' users' filterlists' admin' callback =
  integrationalTestExpectation $ do
    filterlistContract' <-
      lOriginate
        (Filterlist.filterlistContract @Address)
        "Filterlist Contract"
        (Filterlist.Storage
           issuer'
           (Filterlist.mkUsers users')
           (Filterlist.mkFilterlists filterlists')
           admin')
        (toMutez 0)
    callback filterlistContract'

-- assertTransfer :: ()
--   => String
--   -> Bool
--   -> Address
--   -> [(Address, Filterlist.FilterlistId)]
--   -> [(Filterlist.FilterlistId, (Bool, [Filterlist.FilterlistId]))]
--   -> Address
--   -> Address
--   -> Address
--   -> TestTree
-- assertTransfer description' shouldSucceed' issuer' users' filterlists' admin' from' to' =
--   testCase description' $
--   withFilterlistContract
--     issuer'
--     users'
--     filterlists'
--     admin' $ \filterlistContract' -> do
--       lCall filterlistContract' . Filterlist.AssertTransfer $ Filterlist.TransferParams from' to'
--       if shouldSucceed'
--          then validate . Right $
--            expectAnySuccess
--          else validate . Left $
--            lExpectMichelsonFailed (const True) filterlistContract'

-- test_AssertTransfer :: TestTree
-- test_AssertTransfer = testGroup "AssertTransfer"
--   [ assertTransfer "Assert transfer with no users" shouldFail
--       genesisAddress1 mempty mempty genesisAddress2 genesisAddress3 genesisAddress4
--   , assertTransfer "Assert transfer from issuer with no users" shouldFail
--       genesisAddress1 mempty mempty genesisAddress2 genesisAddress1 genesisAddress1
--   , assertTransfer "Assert transfer from admin with no users" shouldFail
--       genesisAddress1 mempty mempty genesisAddress2 genesisAddress2 genesisAddress2
--   , assertTransfer "Assert transfer from issuer to a user" shouldSucceed
--       genesisAddress1 [(genesisAddress3, 0)] [(0, (True, []))] genesisAddress2 genesisAddress1 genesisAddress3
--   , assertTransfer "Assert transfer from filterlisted user to self (not on own filterlist)" shouldFail
--       genesisAddress1 [(genesisAddress3, 0)] [(0, (True, []))] genesisAddress2 genesisAddress3 genesisAddress3
--   , assertTransfer "Assert transfer from filterlisted user to self (on own filterlist)" shouldSucceed
--       genesisAddress1 [(genesisAddress3, 0)] [(0, (True, [0]))] genesisAddress2 genesisAddress3 genesisAddress3
--   , assertTransfer "Assert transfer from filterlisted user to self (on own filterlist, restricted)" shouldFail
--       genesisAddress1 [(genesisAddress3, 0)] [(0, (False, [0]))] genesisAddress2 genesisAddress3 genesisAddress3
--   , assertTransfer "Assert transfer from filterlisted user to other user on same filterlist (no outbound)" shouldFail
--       genesisAddress1 [(genesisAddress3, 0), (genesisAddress4, 0)] [(0, (True, []))] genesisAddress2 genesisAddress3 genesisAddress4
--   , assertTransfer "Assert transfer from filterlisted user to other user on same filterlist (with outbound)" shouldSucceed
--       genesisAddress1 [(genesisAddress3, 0), (genesisAddress4, 0)] [(0, (True, [0]))] genesisAddress2 genesisAddress3 genesisAddress4
--   , assertTransfer "Assert transfer from filterlisted user to other user on other filterlist (no outbound)" shouldFail
--       genesisAddress1 [(genesisAddress3, 0), (genesisAddress4, 1)] [(0, (True, [])), (1, (True, []))] genesisAddress2 genesisAddress3 genesisAddress4
--   , assertTransfer "Assert transfer from filterlisted user to other user on other filterlist (with outbound)" shouldSucceed
--       genesisAddress1 [(genesisAddress3, 0), (genesisAddress4, 1)] [(0, (True, [1])), (1, (True, []))] genesisAddress2 genesisAddress3 genesisAddress4
--   ]

assertTransfers :: ()
  => String
  -> Bool
  -> Address
  -> [(Address, Filterlist.FilterlistId)]
  -> [(Filterlist.FilterlistId, (Bool, [Filterlist.FilterlistId]))]
  -> Address
  -> [(Address, Address)]
  -> TestTree
assertTransfers description' shouldSucceed' issuer' users' filterlists' admin' transfers' =
  testCase description' $
  withFilterlistContract
    issuer'
    users'
    filterlists'
    admin' $ \filterlistContract' -> do
      lCall filterlistContract' $ Filterlist.AssertTransfers $ uncurry Filterlist.TransferParams <$> transfers'
      if shouldSucceed'
         then validate . Right $
           expectAnySuccess
         else validate . Left $
           lExpectMichelsonFailed (const True) filterlistContract'

test_AssertTransfers :: TestTree
test_AssertTransfers = testGroup "AssertTransfers"
  [ assertTransfers "Assert transfer with no users" shouldFail
      genesisAddress1 mempty mempty genesisAddress2 [(genesisAddress3, genesisAddress4)]
  , assertTransfers "Assert transfer from issuer with no users" shouldFail
      genesisAddress1 mempty mempty genesisAddress2 [(genesisAddress1, genesisAddress1)]
  , assertTransfers "Assert transfer from issuer with no users (2x)" shouldFail
      genesisAddress1 mempty mempty genesisAddress2 $ replicate 2 (genesisAddress1, genesisAddress1)
  , assertTransfers "Assert transfer from issuer with no users (3x)" shouldFail
      genesisAddress1 mempty mempty genesisAddress2 $ replicate 3 (genesisAddress1, genesisAddress1)
  , assertTransfers "Assert transfer from admin with no users" shouldFail
      genesisAddress1 mempty mempty genesisAddress2 [(genesisAddress2, genesisAddress2)]
  , assertTransfers "Assert transfer from issuer to a user" shouldSucceed
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, []))] genesisAddress2 [(genesisAddress1, genesisAddress3)]
  , assertTransfers "Assert transfer from issuer to a user (2x)" shouldSucceed
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, []))] genesisAddress2 $ replicate 2 (genesisAddress1, genesisAddress3)
  , assertTransfers "Assert transfer from issuer to a user (3x)" shouldSucceed
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, []))] genesisAddress2 $ replicate 3 (genesisAddress1, genesisAddress3)
  , assertTransfers "Assert transfer from issuer to a user (3x), then from non-user" shouldFail
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, []))] genesisAddress2 $ replicate 3 (genesisAddress1, genesisAddress3) ++ [(genesisAddress4, genesisAddress4)]
  , assertTransfers "Assert transfer from filterlisted user to self (not on own filterlist)" shouldFail
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, []))] genesisAddress2 [(genesisAddress3, genesisAddress3)]
  , assertTransfers "Assert transfer from filterlisted user to self (on own filterlist)" shouldSucceed
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, [0]))] genesisAddress2 [(genesisAddress3, genesisAddress3)]
  , assertTransfers "Assert transfer from filterlisted user to self (on own filterlist) (2x)" shouldSucceed
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, [0]))] genesisAddress2 $ replicate 2 (genesisAddress3, genesisAddress3)
  , assertTransfers "Assert transfer from filterlisted user to self (on own filterlist) (3x)" shouldSucceed
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, [0]))] genesisAddress2 $ replicate 3 (genesisAddress3, genesisAddress3)
  , assertTransfers "Assert transfer from filterlisted user to self (on own filterlist) (3x), then to non-user" shouldFail
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, [0]))] genesisAddress2 $ replicate 3 (genesisAddress3, genesisAddress3) ++ [(genesisAddress3, genesisAddress4)]
  , assertTransfers "Assert transfer from filterlisted user to self (on own filterlist, restricted)" shouldFail
      genesisAddress1 [(genesisAddress3, 0)] [(0, (False, [0]))] genesisAddress2 [(genesisAddress3, genesisAddress3)]
  ]

-- test_AssertReceiver :: TestTree
-- test_AssertReceiver = testGroup "AssertReceiver"
--   [ testCase "Assert receiver with no users" $ do
--       withFilterlistContract
--         genesisAddress1
--         mempty
--         mempty
--         genesisAddress2 $ \filterlistContract' -> do
--           lCall filterlistContract' $ Filterlist.AssertReceiver genesisAddress3
--           validate . Left $
--             lExpectMichelsonFailed (const True) filterlistContract'
--   , testCase "Assert receiver issuer with no users" $ do
--       withFilterlistContract
--         genesisAddress1
--         mempty
--         mempty
--         genesisAddress2 $ \filterlistContract' -> do
--           lCall filterlistContract' $ Filterlist.AssertReceiver genesisAddress1
--           validate . Left $
--             lExpectMichelsonFailed (const True) filterlistContract'
--   , testCase "Assert receiver admin with no users" $ do
--       withFilterlistContract
--         genesisAddress1
--         mempty
--         mempty
--         genesisAddress2 $ \filterlistContract' -> do
--           lCall filterlistContract' $ Filterlist.AssertReceiver genesisAddress2
--           validate . Left $
--             lExpectMichelsonFailed (const True) filterlistContract'
--   , testCase "Assert receiver issuer with user" $ do
--       withFilterlistContract
--         genesisAddress1
--         [(genesisAddress3, 0)]
--         [(0, (True, []))]
--         genesisAddress2 $ \filterlistContract' -> do
--           lCall filterlistContract' $ Filterlist.AssertReceiver genesisAddress1
--           validate . Left $
--             lExpectMichelsonFailed (const True) filterlistContract'
--   , testCase "Assert receiver on filterlisted user (not on own filterlist)" $ do
--       withFilterlistContract
--         genesisAddress1
--         [(genesisAddress3, 0)]
--         [(0, (True, []))]
--         genesisAddress2 $ \filterlistContract' -> do
--           lCall filterlistContract' $ Filterlist.AssertReceiver genesisAddress3
--           validate . Right $ expectAnySuccess
--   , testCase "Assert receiver on filterlisted user (on own filterlist)" $ do
--       withFilterlistContract
--         genesisAddress1
--         [(genesisAddress3, 0)]
--         [(0, (True, [0]))]
--         genesisAddress2 $ \filterlistContract' -> do
--           lCall filterlistContract' $ Filterlist.AssertReceiver genesisAddress3
--           validate . Right $ expectAnySuccess
--   , testCase "Assert receiver on filterlisted user (on own filterlist, restricted)" $ do
--       withFilterlistContract
--         genesisAddress1
--         [(genesisAddress3, 0)]
--         [(0, (False, [0]))]
--         genesisAddress2 $ \filterlistContract' -> do
--           lCall filterlistContract' $ Filterlist.AssertReceiver genesisAddress3
--           validate . Left $
--             lExpectMichelsonFailed (const True) filterlistContract'
--   ]

test_AssertReceivers :: TestTree
test_AssertReceivers = testGroup "AssertReceivers"
  [ testCase "Assert receivers with no users" $ do
      withFilterlistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \filterlistContract' -> do
          lCall filterlistContract' $ Filterlist.AssertReceivers . return $ genesisAddress3
          validate . Left $
            lExpectMichelsonFailed (const True) filterlistContract'
  , testCase "Assert receivers issuer with no users" $ do
      withFilterlistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \filterlistContract' -> do
          lCall filterlistContract' $ Filterlist.AssertReceivers . return $ genesisAddress1
          validate . Left $
            lExpectMichelsonFailed (const True) filterlistContract'
  , testCase "Assert receivers admin with no users" $ do
      withFilterlistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \filterlistContract' -> do
          lCall filterlistContract' $ Filterlist.AssertReceivers . return $ genesisAddress2
          validate . Left $
            lExpectMichelsonFailed (const True) filterlistContract'
  , testCase "Assert receivers issuer with user" $ do
      withFilterlistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        [(0, (True, []))]
        genesisAddress2 $ \filterlistContract' -> do
          lCall filterlistContract' $ Filterlist.AssertReceivers . return $ genesisAddress1
          validate . Left $
            lExpectMichelsonFailed (const True) filterlistContract'
  , testCase "Assert receivers on filterlisted user (not on own filterlist)" $ do
      withFilterlistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        [(0, (True, []))]
        genesisAddress2 $ \filterlistContract' -> do
          lCall filterlistContract' $ Filterlist.AssertReceivers . return $ genesisAddress3
          validate . Right $ expectAnySuccess
  , testCase "Assert receivers on filterlisted user (on own filterlist)" $ do
      withFilterlistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        [(0, (True, [0]))]
        genesisAddress2 $ \filterlistContract' -> do
          lCall filterlistContract' $ Filterlist.AssertReceivers . return $ genesisAddress3
          validate . Right $ expectAnySuccess
  , testCase "Assert receivers on filterlisted user (on own filterlist, restricted)" $ do
      withFilterlistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        [(0, (False, [0]))]
        genesisAddress2 $ \filterlistContract' -> do
          lCall filterlistContract' $ Filterlist.AssertReceivers . return $ genesisAddress3
          validate . Left $
            lExpectMichelsonFailed (const True) filterlistContract'
  , testCase "Assert receivers with empty list" $ do
      withFilterlistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \filterlistContract' -> do
          lCall filterlistContract' $ Filterlist.AssertReceivers []
          validate . Right $ expectAnySuccess
  , testCase "Assert receivers with multiple valid" $ do
      withFilterlistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        [(0, (True, []))]
        genesisAddress2 $ \filterlistContract' -> do
          lCall filterlistContract' $ Filterlist.AssertReceivers $ replicate 10 genesisAddress3
          validate . Right $ expectAnySuccess
  , testCase "Assert receivers with multiple valid and one invalid" $ do
      withFilterlistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        [(0, (True, []))]
        genesisAddress2 $ \filterlistContract' -> do
          lCall filterlistContract' $ Filterlist.AssertReceivers $ replicate 10 genesisAddress3 ++ [genesisAddress4]
          validate . Left $
            lExpectMichelsonFailed (const True) filterlistContract'
  ]

test_SetIssuer :: TestTree
test_SetIssuer = testGroup "SetIssuer"
  [ testCase "Set issuer as non-admin" $ do
      withFilterlistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \filterlistContract' -> do
          lCall filterlistContract' . Filterlist.OtherParameter $ Filterlist.SetIssuer genesisAddress3
          validate . Left $
            lExpectMichelsonFailed (const True) filterlistContract'
  , testCase "Set issuer as admin" $ do
      withFilterlistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress $ \filterlistContract' -> do
          lCall filterlistContract' . Filterlist.OtherParameter $ Filterlist.SetIssuer genesisAddress3
          validate . Right $ lExpectStorageUpdate @(Filterlist.Storage Address) filterlistContract' $ \st ->
            if Filterlist.issuer st == genesisAddress3
               then return ()
               else Left $ CustomValidationError "Issuer was not updated"
  ]


test_SetAdmin :: TestTree
test_SetAdmin = testGroup "SetAdmin"
  [ testCase "Set admin as non-admin" $ do
      withFilterlistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \filterlistContract' -> do
          lCall filterlistContract' . Filterlist.OtherParameter $ Filterlist.SetAdmin genesisAddress3
          validate . Left $
            lExpectMichelsonFailed (const True) filterlistContract'
  , testCase "Set admin as admin" $ do
      withFilterlistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress $ \filterlistContract' -> do
          lCall filterlistContract' . Filterlist.OtherParameter $ Filterlist.SetAdmin genesisAddress3
          validate . Right $ lExpectStorageUpdate @(Filterlist.Storage Address) filterlistContract' $ \st ->
            if Filterlist.admin st == genesisAddress3
               then return ()
               else Left $ CustomValidationError "Admin was not updated"
  ]

test_UpdateUser :: TestTree
test_UpdateUser = testGroup "UpdateUser"
  [ testCase "Update user as non-admin" $ do
      withFilterlistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \filterlistContract' -> do
          lCall filterlistContract' . Filterlist.OtherParameter $ Filterlist.UpdateUser $ Filterlist.UpdateUserParams genesisAddress3 Nothing
          validate . Left $
            lExpectMichelsonFailed (const True) filterlistContract'
  , testCase "Update user as admin" $ do
      withFilterlistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress $ \filterlistContract' -> do
          lCall filterlistContract' . Filterlist.OtherParameter $ Filterlist.UpdateUser $ Filterlist.UpdateUserParams genesisAddress3 Nothing
          validate . Right $ expectAnySuccess
  , testCase "Add user as admin" $ do
      withFilterlistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress $ \filterlistContract' -> do
          lCall filterlistContract' . Filterlist.OtherParameter $ Filterlist.UpdateUser $ Filterlist.UpdateUserParams genesisAddress3 (Just 0)
          validate . Right $ lExpectStorageUpdate @(Filterlist.Storage Address) filterlistContract' $ \st ->
            if Map.assocs (unBigMap $ Filterlist.users st) == [(genesisAddress3, 0)]
               then return ()
               else Left $ CustomValidationError "User was not added"
  , testCase "Add existing user as admin" $ do
      withFilterlistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        mempty
        genesisAddress $ \filterlistContract' -> do
          lCall filterlistContract' . Filterlist.OtherParameter $ Filterlist.UpdateUser $ Filterlist.UpdateUserParams genesisAddress3 (Just 0)
          validate . Right $ expectNoStorageUpdates
  , testCase "Update existing user as admin" $ do
      withFilterlistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        mempty
        genesisAddress $ \filterlistContract' -> do
          lCall filterlistContract' . Filterlist.OtherParameter $ Filterlist.UpdateUser $ Filterlist.UpdateUserParams genesisAddress3 (Just 1)
          validate . Right $ lExpectStorageUpdate @(Filterlist.Storage Address) filterlistContract' $ \st ->
            if Map.assocs (unBigMap $ Filterlist.users st) == [(genesisAddress3, 1)]
               then return ()
               else Left $ CustomValidationError "User was not updated"
  , testCase "Remove user as admin" $ do
      withFilterlistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        mempty
        genesisAddress $ \filterlistContract' -> do
          lCall filterlistContract' . Filterlist.OtherParameter $ Filterlist.UpdateUser $ Filterlist.UpdateUserParams genesisAddress3 Nothing
          validate . Right $ lExpectStorageUpdate @(Filterlist.Storage Address) filterlistContract' $ \st ->
            if Filterlist.users st == mempty
               then return ()
               else Left $ CustomValidationError "User was not removed"
  ]

test_SetFilterlistOutbound :: TestTree
test_SetFilterlistOutbound = testGroup "SetFilterlistOutbound"
  [ testCase "Update outbound filterlists as non-admin" $ do
      withFilterlistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \filterlistContract' -> do
          lCall filterlistContract' . Filterlist.OtherParameter $ Filterlist.SetFilterlistOutbound $ Filterlist.FilterlistOutboundParams 0 Nothing
          validate . Left $
            lExpectMichelsonFailed (const True) filterlistContract'
  , testCase "Update outbound filterlists as admin" $ do
      withFilterlistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress $ \filterlistContract' -> do
          lCall filterlistContract' . Filterlist.OtherParameter $ Filterlist.SetFilterlistOutbound $ Filterlist.FilterlistOutboundParams 0 Nothing
          validate . Right $ expectAnySuccess
  , testCase "Add outbound filterlists as admin" $ do
      withFilterlistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress $ \filterlistContract' -> do
          lCall filterlistContract' . Filterlist.OtherParameter $ Filterlist.SetFilterlistOutbound $ Filterlist.FilterlistOutboundParams 0 $ Just $ Filterlist.OutboundFilterlists True mempty
          validate . Right $ lExpectStorageUpdate @(Filterlist.Storage Address) filterlistContract' $ \st ->
            if Map.assocs (unBigMap $ Filterlist.filterlists st) == [(0, Filterlist.OutboundFilterlists True mempty)]
               then return ()
               else Left $ CustomValidationError "User was not added"
  , testCase "Add existing outbound filterlists as admin" $ do
      withFilterlistContract
        genesisAddress1
        mempty
        [(0, (True, []))]
        genesisAddress $ \filterlistContract' -> do
          lCall filterlistContract' . Filterlist.OtherParameter $ Filterlist.SetFilterlistOutbound $ Filterlist.FilterlistOutboundParams 0 $ Just $ Filterlist.OutboundFilterlists True mempty
          validate . Right $ expectNoStorageUpdates
  , testCase "Update existing outbound filterlists as admin" $ do
      withFilterlistContract
        genesisAddress1
        mempty
        [(0, (True, []))]
        genesisAddress $ \filterlistContract' -> do
          lCall filterlistContract' . Filterlist.OtherParameter $ Filterlist.SetFilterlistOutbound $ Filterlist.FilterlistOutboundParams 0 $ Just $ Filterlist.OutboundFilterlists False (Set.singleton 0)
          validate . Right $ lExpectStorageUpdate @(Filterlist.Storage Address) filterlistContract' $ \st ->
            if Map.assocs (unBigMap $ Filterlist.filterlists st) == [(0, Filterlist.OutboundFilterlists False (Set.singleton 0))]
               then return ()
               else Left $ CustomValidationError "User was not updated"
  , testCase "Remove outbound filterlist as admin" $ do
      withFilterlistContract
        genesisAddress1
        mempty
        [(0, (True, []))]
        genesisAddress $ \filterlistContract' -> do
          lCall filterlistContract' . Filterlist.OtherParameter $ Filterlist.SetFilterlistOutbound $ Filterlist.FilterlistOutboundParams 0 Nothing
          validate . Right $ lExpectStorageUpdate @(Filterlist.Storage Address) filterlistContract' $ \st ->
            if Filterlist.filterlists st == mempty
               then return ()
               else Left $ CustomValidationError "User was not removed"
  ]



