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
  -> Whitelist.Users Address
  -> Whitelist.Whitelists
  -> Address
  -> (ContractRef (Whitelist.Parameter Address) -> IntegrationalScenario)
  -> Expectation
withWhitelistContract issuer' users' whitelists' admin' callback =
  integrationalTestExpectation $ do
    whitelistContract' <- lOriginate (Whitelist.whitelistContract @Address) "Whitelist Contract"
              (Whitelist.Storage issuer' users' whitelists' admin')
              (toMutez 0)
    callback whitelistContract'

test_whitelist :: TestTree
test_whitelist = testGroup "Whitelist contract tests"
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
          validate . Right . return . return . return $ return ()
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
        (Whitelist.mkUsers [(genesisAddress3, 0)])
        (Whitelist.mkWhitelists [(0, (True, []))])
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' . Whitelist.AssertTransfer $ Whitelist.TransferParams genesisAddress1 genesisAddress3
          validate . Right . return . return . return $ return ()
  , testCase "Test assert transfer from whitelisted user to self (not on own whitelist)" $ do
      withWhitelistContract
        genesisAddress1
        (Whitelist.mkUsers [(genesisAddress3, 0)])
        (Whitelist.mkWhitelists [(0, (True, []))])
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' . Whitelist.AssertTransfer $ Whitelist.TransferParams genesisAddress3 genesisAddress3
          validate . Left $
            lExpectMichelsonFailed (const True) whitelistContract'
  , testCase "Test assert transfer from whitelisted user to self (on own whitelist)" $ do
      withWhitelistContract
        genesisAddress1
        (Whitelist.mkUsers [(genesisAddress3, 0)])
        (Whitelist.mkWhitelists [(0, (True, [0]))])
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' . Whitelist.AssertTransfer $ Whitelist.TransferParams genesisAddress3 genesisAddress3
          validate . Right . return . return . return $ return ()
  , testCase "Test assert transfer from whitelisted user to self (on own whitelist, restricted)" $ do
      withWhitelistContract
        genesisAddress1
        (Whitelist.mkUsers [(genesisAddress3, 0)])
        (Whitelist.mkWhitelists [(0, (False, [0]))])
        genesisAddress2 $ \whitelistContract' -> do
          lCall whitelistContract' . Whitelist.AssertTransfer $ Whitelist.TransferParams genesisAddress3 genesisAddress3
          validate . Left $
            lExpectMichelsonFailed (const True) whitelistContract'
  ]

