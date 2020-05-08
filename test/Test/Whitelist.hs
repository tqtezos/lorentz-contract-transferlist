module Test.Whitelist where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Test.Hspec (Expectation)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Lorentz
import Lorentz.Test

import qualified Lorentz.Contracts.Whitelist as Whitelist
import qualified Lorentz.Contracts.Whitelist.Types as Whitelist

shouldSucceed :: Bool
shouldSucceed = True

shouldFail :: Bool
shouldFail = False

withWhitelistContract :: ()
  => Address
  -> [(Address, Whitelist.WhitelistId)]
  -> [(Whitelist.WhitelistId, (Bool, [Whitelist.WhitelistId]))]
  -> Address
  -> (TAddress (Whitelist.Parameter Address) -> IntegrationalScenario)
  -> Expectation
withWhitelistContract issuer' users' whitelists' admin' callback =
  integrationalTestExpectation $ do
    whitelistContract' <-
      lOriginate
        (Whitelist.whitelistContract @Address)
        "Whitelist Contract"
        (Whitelist.Storage
           issuer'
           (Whitelist.mkUsers users')
           (Whitelist.mkWhitelists whitelists')
           admin')
        (toMutez 0)
    callback whitelistContract'

assertTransfer :: ()
  => String
  -> Bool
  -> Address
  -> [(Address, Whitelist.WhitelistId)]
  -> [(Whitelist.WhitelistId, (Bool, [Whitelist.WhitelistId]))]
  -> Address
  -> Address
  -> Address
  -> TestTree
assertTransfer description' shouldSucceed' issuer' users' whitelists' admin' from' to' =
  testCase description' $
  withWhitelistContract
    issuer'
    users'
    whitelists'
    admin' $ \whitelistContract' -> do
      lCall whitelistContract' . Whitelist.AssertTransfer $ Whitelist.TransferParams from' to'
      if shouldSucceed'
         then validate . Right $
           expectAnySuccess
         else validate . Left $
           lExpectMichelsonFailed (const True) whitelistContract'

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
  , assertTransfer "Assert transfer from whitelisted user to self (not on own whitelist)" shouldFail
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, []))] genesisAddress2 genesisAddress3 genesisAddress3
  , assertTransfer "Assert transfer from whitelisted user to self (on own whitelist)" shouldSucceed
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, [0]))] genesisAddress2 genesisAddress3 genesisAddress3
  , assertTransfer "Assert transfer from whitelisted user to self (on own whitelist, restricted)" shouldFail
      genesisAddress1 [(genesisAddress3, 0)] [(0, (False, [0]))] genesisAddress2 genesisAddress3 genesisAddress3
  ]

assertTransfers :: ()
  => String
  -> Bool
  -> Address
  -> [(Address, Whitelist.WhitelistId)]
  -> [(Whitelist.WhitelistId, (Bool, [Whitelist.WhitelistId]))]
  -> Address
  -> [(Address, Address)]
  -> TestTree
assertTransfers description' shouldSucceed' issuer' users' whitelists' admin' transfers' =
  testCase description' $
  withWhitelistContract
    issuer'
    users'
    whitelists'
    admin' $ \whitelistContract' -> do
      lCall whitelistContract' $ Whitelist.AssertTransfers $ uncurry Whitelist.TransferParams <$> transfers'
      if shouldSucceed'
         then validate . Right $
           expectAnySuccess
         else validate . Left $
           lExpectMichelsonFailed (const True) whitelistContract'

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
  , assertTransfers "Assert transfer from whitelisted user to self (not on own whitelist)" shouldFail
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, []))] genesisAddress2 [(genesisAddress3, genesisAddress3)]
  , assertTransfers "Assert transfer from whitelisted user to self (on own whitelist)" shouldSucceed
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, [0]))] genesisAddress2 [(genesisAddress3, genesisAddress3)]
  , assertTransfers "Assert transfer from whitelisted user to self (on own whitelist) (2x)" shouldSucceed
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, [0]))] genesisAddress2 $ replicate 2 (genesisAddress3, genesisAddress3)
  , assertTransfers "Assert transfer from whitelisted user to self (on own whitelist) (3x)" shouldSucceed
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, [0]))] genesisAddress2 $ replicate 3 (genesisAddress3, genesisAddress3)
  , assertTransfers "Assert transfer from whitelisted user to self (on own whitelist) (3x), then to non-user" shouldFail
      genesisAddress1 [(genesisAddress3, 0)] [(0, (True, [0]))] genesisAddress2 $ replicate 3 (genesisAddress3, genesisAddress3) ++ [(genesisAddress3, genesisAddress4)]
  , assertTransfers "Assert transfer from whitelisted user to self (on own whitelist, restricted)" shouldFail
      genesisAddress1 [(genesisAddress3, 0)] [(0, (False, [0]))] genesisAddress2 [(genesisAddress3, genesisAddress3)]
  ]

test_AssertReceiver :: TestTree
test_AssertReceiver = testGroup "AssertReceiver"
  [ testCase "Assert receiver with no users" $ do
      withWhitelistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' $ Whitelist.AssertReceiver genesisAddress3
          validate . Left $
            lExpectMichelsonFailed (const True) whitelistContract'
  , testCase "Assert receiver issuer with no users" $ do
      withWhitelistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' $ Whitelist.AssertReceiver genesisAddress1
          validate . Left $
            lExpectMichelsonFailed (const True) whitelistContract'
  , testCase "Assert receiver admin with no users" $ do
      withWhitelistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' $ Whitelist.AssertReceiver genesisAddress2
          validate . Left $
            lExpectMichelsonFailed (const True) whitelistContract'
  , testCase "Assert receiver issuer with user" $ do
      withWhitelistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        [(0, (True, []))]
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' $ Whitelist.AssertReceiver genesisAddress1
          validate . Left $
            lExpectMichelsonFailed (const True) whitelistContract'
  , testCase "Assert receiver on whitelisted user (not on own whitelist)" $ do
      withWhitelistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        [(0, (True, []))]
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' $ Whitelist.AssertReceiver genesisAddress3
          validate . Right $ expectAnySuccess
  , testCase "Assert receiver on whitelisted user (on own whitelist)" $ do
      withWhitelistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        [(0, (True, [0]))]
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' $ Whitelist.AssertReceiver genesisAddress3
          validate . Right $ expectAnySuccess
  , testCase "Assert receiver on whitelisted user (on own whitelist, restricted)" $ do
      withWhitelistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        [(0, (False, [0]))]
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' $ Whitelist.AssertReceiver genesisAddress3
          validate . Left $
            lExpectMichelsonFailed (const True) whitelistContract'
  ]

test_AssertReceivers :: TestTree
test_AssertReceivers = testGroup "AssertReceivers"
  [ testCase "Assert receivers with no users" $ do
      withWhitelistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' $ Whitelist.AssertReceivers . return $ genesisAddress3
          validate . Left $
            lExpectMichelsonFailed (const True) whitelistContract'
  , testCase "Assert receivers issuer with no users" $ do
      withWhitelistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' $ Whitelist.AssertReceivers . return $ genesisAddress1
          validate . Left $
            lExpectMichelsonFailed (const True) whitelistContract'
  , testCase "Assert receivers admin with no users" $ do
      withWhitelistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' $ Whitelist.AssertReceivers . return $ genesisAddress2
          validate . Left $
            lExpectMichelsonFailed (const True) whitelistContract'
  , testCase "Assert receivers issuer with user" $ do
      withWhitelistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        [(0, (True, []))]
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' $ Whitelist.AssertReceivers . return $ genesisAddress1
          validate . Left $
            lExpectMichelsonFailed (const True) whitelistContract'
  , testCase "Assert receivers on whitelisted user (not on own whitelist)" $ do
      withWhitelistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        [(0, (True, []))]
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' $ Whitelist.AssertReceivers . return $ genesisAddress3
          validate . Right $ expectAnySuccess
  , testCase "Assert receivers on whitelisted user (on own whitelist)" $ do
      withWhitelistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        [(0, (True, [0]))]
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' $ Whitelist.AssertReceivers . return $ genesisAddress3
          validate . Right $ expectAnySuccess
  , testCase "Assert receivers on whitelisted user (on own whitelist, restricted)" $ do
      withWhitelistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        [(0, (False, [0]))]
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' $ Whitelist.AssertReceivers . return $ genesisAddress3
          validate . Left $
            lExpectMichelsonFailed (const True) whitelistContract'
  , testCase "Assert receivers with empty list" $ do
      withWhitelistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' $ Whitelist.AssertReceivers []
          validate . Right $ expectAnySuccess
  , testCase "Assert receivers with multiple valid" $ do
      withWhitelistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        [(0, (True, []))]
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' $ Whitelist.AssertReceivers $ replicate 10 genesisAddress3
          validate . Right $ expectAnySuccess
  , testCase "Assert receivers with multiple valid and one invalid" $ do
      withWhitelistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        [(0, (True, []))]
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' $ Whitelist.AssertReceivers $ replicate 10 genesisAddress3 ++ [genesisAddress4]
          validate . Left $
            lExpectMichelsonFailed (const True) whitelistContract'
  ]

test_SetIssuer :: TestTree
test_SetIssuer = testGroup "SetIssuer"
  [ testCase "Set issuer as non-admin" $ do
      withWhitelistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' . Whitelist.OtherParameter $ Whitelist.SetIssuer genesisAddress3
          validate . Left $
            lExpectMichelsonFailed (const True) whitelistContract'
  , testCase "Set issuer as admin" $ do
      withWhitelistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress $ \whitelistContract' -> do
          lCall whitelistContract' . Whitelist.OtherParameter $ Whitelist.SetIssuer genesisAddress3
          validate . Right $ lExpectStorageUpdate @(Whitelist.Storage Address) whitelistContract' $ \st ->
            if Whitelist.issuer st == genesisAddress3
               then return ()
               else Left $ CustomValidationError "Issuer was not updated"
  ]


test_SetAdmin :: TestTree
test_SetAdmin = testGroup "SetAdmin"
  [ testCase "Set admin as non-admin" $ do
      withWhitelistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' . Whitelist.OtherParameter $ Whitelist.SetAdmin genesisAddress3
          validate . Left $
            lExpectMichelsonFailed (const True) whitelistContract'
  , testCase "Set admin as admin" $ do
      withWhitelistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress $ \whitelistContract' -> do
          lCall whitelistContract' . Whitelist.OtherParameter $ Whitelist.SetAdmin genesisAddress3
          validate . Right $ lExpectStorageUpdate @(Whitelist.Storage Address) whitelistContract' $ \st ->
            if Whitelist.admin st == genesisAddress3
               then return ()
               else Left $ CustomValidationError "Admin was not updated"
  ]

test_AddUser :: TestTree
test_AddUser = testGroup "AddUser"
  [ testCase "Update user as non-admin" $ do
      withWhitelistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' . Whitelist.OtherParameter $ Whitelist.AddUser $ Whitelist.UpdateUserParams genesisAddress3 Nothing
          validate . Left $
            lExpectMichelsonFailed (const True) whitelistContract'
  , testCase "Update user as admin" $ do
      withWhitelistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress $ \whitelistContract' -> do
          lCall whitelistContract' . Whitelist.OtherParameter $ Whitelist.AddUser $ Whitelist.UpdateUserParams genesisAddress3 Nothing
          validate . Right $ expectAnySuccess
  , testCase "Add user as admin" $ do
      withWhitelistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress $ \whitelistContract' -> do
          lCall whitelistContract' . Whitelist.OtherParameter $ Whitelist.AddUser $ Whitelist.UpdateUserParams genesisAddress3 (Just 0)
          validate . Right $ lExpectStorageUpdate @(Whitelist.Storage Address) whitelistContract' $ \st ->
            if Map.assocs (unBigMap $ Whitelist.users st) == [(genesisAddress3, 0)]
               then return ()
               else Left $ CustomValidationError "User was not added"
  , testCase "Add existing user as admin" $ do
      withWhitelistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        mempty
        genesisAddress $ \whitelistContract' -> do
          lCall whitelistContract' . Whitelist.OtherParameter $ Whitelist.AddUser $ Whitelist.UpdateUserParams genesisAddress3 (Just 0)
          validate . Right $ expectNoStorageUpdates
  , testCase "Update existing user as admin" $ do
      withWhitelistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        mempty
        genesisAddress $ \whitelistContract' -> do
          lCall whitelistContract' . Whitelist.OtherParameter $ Whitelist.AddUser $ Whitelist.UpdateUserParams genesisAddress3 (Just 1)
          validate . Right $ lExpectStorageUpdate @(Whitelist.Storage Address) whitelistContract' $ \st ->
            if Map.assocs (unBigMap $ Whitelist.users st) == [(genesisAddress3, 1)]
               then return ()
               else Left $ CustomValidationError "User was not updated"
  , testCase "Remove user as admin" $ do
      withWhitelistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        mempty
        genesisAddress $ \whitelistContract' -> do
          lCall whitelistContract' . Whitelist.OtherParameter $ Whitelist.AddUser $ Whitelist.UpdateUserParams genesisAddress3 Nothing
          validate . Right $ lExpectStorageUpdate @(Whitelist.Storage Address) whitelistContract' $ \st ->
            if Whitelist.users st == mempty
               then return ()
               else Left $ CustomValidationError "User was not removed"
  ]

test_SetWhitelistOutbound :: TestTree
test_SetWhitelistOutbound = testGroup "SetWhitelistOutbound"
  [ testCase "Update outbound whitelists as non-admin" $ do
      withWhitelistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' . Whitelist.OtherParameter $ Whitelist.SetWhitelistOutbound $ Whitelist.WhitelistOutboundParams 0 Nothing
          validate . Left $
            lExpectMichelsonFailed (const True) whitelistContract'
  , testCase "Update outbound whitelists as admin" $ do
      withWhitelistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress $ \whitelistContract' -> do
          lCall whitelistContract' . Whitelist.OtherParameter $ Whitelist.SetWhitelistOutbound $ Whitelist.WhitelistOutboundParams 0 Nothing
          validate . Right $ expectAnySuccess
  , testCase "Add outbound whitelists as admin" $ do
      withWhitelistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress $ \whitelistContract' -> do
          lCall whitelistContract' . Whitelist.OtherParameter $ Whitelist.SetWhitelistOutbound $ Whitelist.WhitelistOutboundParams 0 $ Just $ Whitelist.OutboundWhitelists True mempty
          validate . Right $ lExpectStorageUpdate @(Whitelist.Storage Address) whitelistContract' $ \st ->
            if Map.assocs (unBigMap $ Whitelist.whitelists st) == [(0, Whitelist.OutboundWhitelists True mempty)]
               then return ()
               else Left $ CustomValidationError "User was not added"
  , testCase "Add existing outbound whitelists as admin" $ do
      withWhitelistContract
        genesisAddress1
        mempty
        [(0, (True, []))]
        genesisAddress $ \whitelistContract' -> do
          lCall whitelistContract' . Whitelist.OtherParameter $ Whitelist.SetWhitelistOutbound $ Whitelist.WhitelistOutboundParams 0 $ Just $ Whitelist.OutboundWhitelists True mempty
          validate . Right $ expectNoStorageUpdates
  , testCase "Update existing outbound whitelists as admin" $ do
      withWhitelistContract
        genesisAddress1
        mempty
        [(0, (True, []))]
        genesisAddress $ \whitelistContract' -> do
          lCall whitelistContract' . Whitelist.OtherParameter $ Whitelist.SetWhitelistOutbound $ Whitelist.WhitelistOutboundParams 0 $ Just $ Whitelist.OutboundWhitelists False (Set.singleton 0)
          validate . Right $ lExpectStorageUpdate @(Whitelist.Storage Address) whitelistContract' $ \st ->
            if Map.assocs (unBigMap $ Whitelist.whitelists st) == [(0, Whitelist.OutboundWhitelists False (Set.singleton 0))]
               then return ()
               else Left $ CustomValidationError "User was not updated"
  , testCase "Remove outbound whitelist as admin" $ do
      withWhitelistContract
        genesisAddress1
        mempty
        [(0, (True, []))]
        genesisAddress $ \whitelistContract' -> do
          lCall whitelistContract' . Whitelist.OtherParameter $ Whitelist.SetWhitelistOutbound $ Whitelist.WhitelistOutboundParams 0 Nothing
          validate . Right $ lExpectStorageUpdate @(Whitelist.Storage Address) whitelistContract' $ \st ->
            if Whitelist.whitelists st == mempty
               then return ()
               else Left $ CustomValidationError "User was not removed"
  ]



