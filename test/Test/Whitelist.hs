module Test.Whitelist where

import Test.Hspec (Expectation)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Lorentz
import Lorentz.Test.Integrational

import qualified Lorentz.Contracts.Whitelist as Whitelist
import qualified Lorentz.Contracts.Whitelist.Types as Whitelist

withWhitelistContract :: ()
  => Address
  -> [(Address, Whitelist.WhitelistId)]
  -> [(Whitelist.WhitelistId, (Bool, [Whitelist.WhitelistId]))]
  -> Address
  -> (ContractRef (Whitelist.Parameter Address) -> IntegrationalScenario)
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

test_AssertTransfer :: TestTree
test_AssertTransfer = testGroup "AssertTransfer"
  [ testCase "Test assert transfer with no users" $ do
      withWhitelistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' . Whitelist.AssertTransfer $ Whitelist.TransferParams genesisAddress3 genesisAddress4
          validate . Left $
            lExpectMichelsonFailed (const True) whitelistContract'
  , testCase "Test assert transfer from issuer with no users" $ do
      withWhitelistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' . Whitelist.AssertTransfer $ Whitelist.TransferParams genesisAddress1 genesisAddress1
          validate . Right $ expectAnySuccess
  , testCase "Test assert transfer from admin with no users" $ do
      withWhitelistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' . Whitelist.AssertTransfer $ Whitelist.TransferParams genesisAddress2 genesisAddress2
          validate . Left $
            lExpectMichelsonFailed (const True) whitelistContract'
  , testCase "Test assert transfer from issuer to a user" $ do
      withWhitelistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        [(0, (True, []))]
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' . Whitelist.AssertTransfer $ Whitelist.TransferParams genesisAddress1 genesisAddress3
          validate . Right $ expectAnySuccess
  , testCase "Test assert transfer from whitelisted user to self (not on own whitelist)" $ do
      withWhitelistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        [(0, (True, []))]
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' . Whitelist.AssertTransfer $ Whitelist.TransferParams genesisAddress3 genesisAddress3
          validate . Left $
            lExpectMichelsonFailed (const True) whitelistContract'
  , testCase "Test assert transfer from whitelisted user to self (on own whitelist)" $ do
      withWhitelistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        [(0, (True, [0]))]
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' . Whitelist.AssertTransfer $ Whitelist.TransferParams genesisAddress3 genesisAddress3
          validate . Right $ expectAnySuccess
  , testCase "Test assert transfer from whitelisted user to self (on own whitelist, restricted)" $ do
      withWhitelistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        [(0, (False, [0]))]
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' . Whitelist.AssertTransfer $ Whitelist.TransferParams genesisAddress3 genesisAddress3
          validate . Left $
            lExpectMichelsonFailed (const True) whitelistContract'
  ]

test_AssertReceiver :: TestTree
test_AssertReceiver = testGroup "AssertReceiver"
  [ testCase "Test assert receiver with no users" $ do
      withWhitelistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' $ Whitelist.AssertReceiver genesisAddress3
          validate . Left $
            lExpectMichelsonFailed (const True) whitelistContract'
  , testCase "Test assert receiver issuer with no users" $ do
      withWhitelistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' $ Whitelist.AssertReceiver genesisAddress1
          validate . Right $ expectAnySuccess
  , testCase "Test assert receiver admin with no users" $ do
      withWhitelistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' $ Whitelist.AssertReceiver genesisAddress2
          validate . Left $
            lExpectMichelsonFailed (const True) whitelistContract'
  , testCase "Test assert receiver issuer with user" $ do
      withWhitelistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        [(0, (True, []))]
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' $ Whitelist.AssertReceiver genesisAddress1
          validate . Right $ expectAnySuccess
  , testCase "Test assert receiver on whitelisted user (not on own whitelist)" $ do
      withWhitelistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        [(0, (True, []))]
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' $ Whitelist.AssertReceiver genesisAddress3
          validate . Right $ expectAnySuccess
  , testCase "Test assert receiver on whitelisted user (on own whitelist)" $ do
      withWhitelistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        [(0, (True, [0]))]
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' $ Whitelist.AssertReceiver genesisAddress3
          validate . Right $ expectAnySuccess
  , testCase "Test assert receiver on whitelisted user (on own whitelist, restricted)" $ do
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
  [ testCase "Test assert receivers with no users" $ do
      withWhitelistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' $ Whitelist.AssertReceivers . return $ genesisAddress3
          validate . Left $
            lExpectMichelsonFailed (const True) whitelistContract'
  , testCase "Test assert receivers issuer with no users" $ do
      withWhitelistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' $ Whitelist.AssertReceivers . return $ genesisAddress1
          validate . Right $ expectAnySuccess
  , testCase "Test assert receivers admin with no users" $ do
      withWhitelistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' $ Whitelist.AssertReceivers . return $ genesisAddress2
          validate . Left $
            lExpectMichelsonFailed (const True) whitelistContract'
  , testCase "Test assert receivers issuer with user" $ do
      withWhitelistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        [(0, (True, []))]
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' $ Whitelist.AssertReceivers . return $ genesisAddress1
          validate . Right $ expectAnySuccess
  , testCase "Test assert receivers on whitelisted user (not on own whitelist)" $ do
      withWhitelistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        [(0, (True, []))]
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' $ Whitelist.AssertReceivers . return $ genesisAddress3
          validate . Right $ expectAnySuccess
  , testCase "Test assert receivers on whitelisted user (on own whitelist)" $ do
      withWhitelistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        [(0, (True, [0]))]
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' $ Whitelist.AssertReceivers . return $ genesisAddress3
          validate . Right $ expectAnySuccess
  , testCase "Test assert receivers on whitelisted user (on own whitelist, restricted)" $ do
      withWhitelistContract
        genesisAddress1
        [(genesisAddress3, 0)]
        [(0, (False, [0]))]
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' $ Whitelist.AssertReceivers . return $ genesisAddress3
          validate . Left $
            lExpectMichelsonFailed (const True) whitelistContract'
  , testCase "Test assert receivers with empty list" $ do
      withWhitelistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' $ Whitelist.AssertReceivers []
          validate . Right $ expectAnySuccess
  , testCase "Test assert receivers with multiple valid" $ do
      withWhitelistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' $ Whitelist.AssertReceivers $ replicate 10 genesisAddress1
          validate . Right $ expectAnySuccess
  , testCase "Test assert receivers with multiple valid and one invalid" $ do
      withWhitelistContract
        genesisAddress1
        mempty
        mempty
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' $ Whitelist.AssertReceivers $ replicate 10 genesisAddress1 ++ [genesisAddress3]
          validate . Left $
            lExpectMichelsonFailed (const True) whitelistContract'
  ]

-- test_whitelist :: TestTree
-- test_whitelist = testGroup "Whitelist contract tests"
--   [ test_AssertTransfer
--   , test_AssertReceiver
--   , test_AssertReceivers
--   ]

