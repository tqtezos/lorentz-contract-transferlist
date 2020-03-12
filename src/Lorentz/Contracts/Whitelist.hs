{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-unused-do-bind -Wno-partial-fields -Wno-orphans #-}

module Lorentz.Contracts.Whitelist where

import Prelude hiding ((>>), drop, swap, get)
import GHC.Generics (Generic, Generic1)
import Text.Show (Show(..))
import Text.Read (Read(..))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Lorentz
import Michelson.Text
import Michelson.Typed.Haskell.Value (IsComparable)
-- import Michelson.Typed.Scope

import Lorentz.Contracts.Util ()

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

data TransferParams a = TransferParams
  { from :: !a
  , to   :: !a
  }
  deriving  (Generic)
  deriving  (Generic1)

deriving instance Read a => Read (TransferParams a)

deriving instance Show a => Show (TransferParams a)

deriving instance IsoValue a => IsoValue (TransferParams a)

toTransferParams :: (a, a) & s :-> TransferParams a & s
toTransferParams = coerce_

unTransferParams :: TransferParams a & s :-> (a, a) & s
unTransferParams = coerce_

data WhitelistOutboundParams = WhitelistOutboundParams
  { whitelist             :: !WhitelistId
  , newOutboundWhitelists :: !(Maybe OutboundWhitelists)
  }
  deriving  (Generic)
  deriving  (Read)
  deriving  (Show)
  deriving  (IsoValue)

unWhitelistOutboundParams :: WhitelistOutboundParams & s :-> WhitelistId & Maybe OutboundWhitelists & s
unWhitelistOutboundParams = do
  coerce_
  unpair

data NewUserParams a = NewUserParams
  { newUser          :: !a
  , newUserWhitelist :: !(Maybe WhitelistId)
  }
  deriving  (Generic)
  deriving  (Generic1)

unNewUserParams :: NewUserParams a & s :-> a & Maybe WhitelistId & s
unNewUserParams = do
  coerce_
  unpair

deriving instance Read a => Read (NewUserParams a)

deriving instance Show a => Show (NewUserParams a)

deriving instance IsoValue a => IsoValue (NewUserParams a)

data Parameter a
  = AssertTransfer
      { transferParams :: !(TransferParams a)
      }
  | OtherParameter
      { otherParams :: !(Parameter' a)
      }
  deriving  (Generic)

instance NiceParameter a => ParameterEntryPoints (Parameter a) where
  parameterEntryPoints = pepNone

deriving instance (NiceParameter a, Read a) => Read (Parameter a)

deriving instance Show a => Show (Parameter a)

deriving instance IsoValue a => IsoValue (Parameter a)

data Parameter' a
  = SetIssuer
      { newIssuer :: !a
      }
  | AddUser
      { newUserParams :: !(NewUserParams a)
      }
  | SetWhitelistOutbound
      { whitelistOutboundParams :: !WhitelistOutboundParams
      }
  | SetAdmin
      { admin :: !Address
      }
  | GetIssuer
      { viewIssuer :: !(View_ a)
      }
  | GetUser
      { viewUser :: !(View a (Maybe WhitelistId))
      }
  | GetWhitelist
      { viewWhitelist :: !(View WhitelistId (Maybe OutboundWhitelists))
      }
  | GetAdmin
      { viewAdmin :: !(View_ Address)
      }
  deriving  (Generic)

deriving instance (NiceParameter a, Read a) => Read (Parameter' a)

deriving instance Show a => Show (Parameter' a)

deriving instance IsoValue a => IsoValue (Parameter' a)

toAssertTransfer :: forall a s. KnownValue a => TransferParams a & s :-> Parameter a & s
toAssertTransfer = do
  left @(TransferParams a) @(Parameter' a)
  coerce_

type View_ = View ()

type WhitelistId = Natural

type Users a = BigMap a WhitelistId

mapUsers :: Ord b => (a -> b) -> Users a -> Users b
mapUsers f = BigMap . Map.mapKeys f . unBigMap

mkUsers :: Ord a => [(a, Natural)] -> Users a
mkUsers = BigMap . Map.fromList

data OutboundWhitelists = OutboundWhitelists
  { unrestricted        :: !Bool
  , allowedWhitelists :: !(Set WhitelistId)
  }
  deriving  (Generic)
  deriving  (Read)
  deriving  (Show)
  deriving  (IsoValue)

unOutboundWhitelists :: OutboundWhitelists & s :-> (Bool, Set WhitelistId) & s
unOutboundWhitelists = coerce_

type Whitelists = BigMap WhitelistId OutboundWhitelists

mkOutboundWhitelists :: Bool -> [Natural] -> OutboundWhitelists
mkOutboundWhitelists unrestricted' =
  OutboundWhitelists unrestricted' .
  Set.fromList

mkWhitelists :: [(Natural, (Bool, [Natural]))] -> Whitelists
mkWhitelists =
  BigMap . fmap (uncurry mkOutboundWhitelists) . Map.fromList

data Storage a =
  Storage
    { issuer :: !a
    , users :: !(Users a)
    , whitelists :: !Whitelists
    , admin :: !Address
    }
  deriving  (Generic)

deriving instance Show a => Show (Storage a)

deriving instance (Ord a, IsoValue a, IsoCValue a) => IsoValue (Storage a)

mapStorage :: Ord b => (a -> b) -> Storage a -> Storage b
mapStorage f xs@Storage{..} = xs
  { issuer = f issuer
  , users = mapUsers f users
  }

-- | Wrap `Storage` with `pair`ing
mkStorage :: a & Users a & Whitelists & Address & s :-> Storage a & s
mkStorage = do
  pair
  dip pair
  pair
  coerce_

-- | Unwrap `Storage`
unStorage :: Storage a & s :-> ((a, Users a), (Whitelists, Address)) & s
unStorage = coerce_

-- | Wrap `Storage`
toStorage :: ((a, Users a), (Whitelists, Address)) & s :-> Storage a & s
toStorage = coerce_

-- | Specialized `update`
addUserWhitelist :: forall a s. IsComparable a => a & Maybe WhitelistId & Users a & s :-> Users a & s
addUserWhitelist = update @(Users a)

-- | Specialized `get`
userWhitelist :: forall a s. IsComparable a => a & Users a & s :-> Maybe WhitelistId & s
userWhitelist = get @(Users a)

-- | Assert that the user is on a whitelist
assertUserWhitelist :: IsComparable a => a & Users a & s :-> WhitelistId & s
assertUserWhitelist = do
  userWhitelist
  assertSome $ mkMTextUnsafe "User not on a whitelist"

-- | Assert that the users are on whitelists
assertUsersWhitelist :: IsComparable a
  => a & a & Users a & s :-> WhitelistId & WhitelistId & s
assertUsersWhitelist = do
  dip $ do
    dip dup
    assertUserWhitelist
    swap
  assertUserWhitelist

-- | Specialized `update`
setOutboundWhitelists :: forall s. WhitelistId & Maybe OutboundWhitelists & Whitelists & s :-> Whitelists & s
setOutboundWhitelists = update @Whitelists

-- | Specialized `get`
outboundWhitelists :: forall s. WhitelistId & Whitelists & s :-> Maybe OutboundWhitelists & s
outboundWhitelists = get @Whitelists

-- | Assert that a `WhitelistId` has associated `OutboundWhitelists`
assertOutboundWhitelists :: WhitelistId & Whitelists & s :-> OutboundWhitelists & s
assertOutboundWhitelists = do
  outboundWhitelists
  assertSome $ mkMTextUnsafe "Whitelist does not exist"

-- | Assert that `OutboundWhitelists` `unrestricted` is `True`
assertUnrestrictedOutboundWhitelists :: OutboundWhitelists & s :-> Set WhitelistId & s
assertUnrestrictedOutboundWhitelists = do
  unOutboundWhitelists
  unpair
  assert $ mkMTextUnsafe "outbound restricted"

whitelistContract :: forall a. (IsComparable a, CompareOpHs a, Typeable a, KnownValue a, NoOperation a)
  => Contract (Parameter a) (Storage a)
whitelistContract = do
  unpair
  caseT @(Parameter a)
    ( #cAssertTransfer /-> assertTransfer
    , #cOtherParameter /->
        caseT @(Parameter' a)
          ( #cSetIssuer /-> setIssuer
          , #cAddUser /-> addUser
          , #cSetWhitelistOutbound /-> setWhitelistOutbound
          , #cSetAdmin /-> setAdmin
          , #cGetIssuer /-> getIssuer
          , #cGetUser /-> getUser
          , #cGetWhitelist /-> getWhitelist
          , #cGetAdmin /-> getAdmin
          )
    )

-- | An easy way to call an instance of a `whitelistContract`
callAssertTransfer :: forall a s. (NiceParameter a) => TransferParams a & Address & s :-> Operation & s
callAssertTransfer = do
  toAssertTransfer
  dip $ do
    contract @(Parameter a)
    assertSome $ mkMTextUnsafe "Expected Whitelist Contract"
    push (toEnum 0 :: Mutez)
  transferTokens

-- | Assert that one user is allowed to transfer to the other
-- assertTransfer :: forall a. (IsComparable a, CompareOpHs a, Typeable a) => Entrypoint (TransferParams a) (Storage a)
assertTransfer :: forall a s. (IsComparable a, CompareOpHs a, Typeable a) => TransferParams a & Storage a & s :-> ([Operation], Storage a) & s
assertTransfer = do
  dip $ do
    dup
    unStorage
    unpair
    dip car
    unpair
  unTransferParams
  swap
  dip $ do
    dup
    car
  ifEq
    (do
      drop
      drop
      drop
    )
    (do
      unpair
      assertUsersWhitelist @a
      swap
      dip $ do
        assertOutboundWhitelists
        assertUnrestrictedOutboundWhitelists
      mem
      assert $ mkMTextUnsafe "outbound not whitelisted"
    )
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
      dip $ assertAdmin
      pair
    cdr
  pair
  pair
  toStorage
  nil
  pair

-- | Assert not equal with an error: @"issuer is not a user"@
assertNotIssuer :: (CompareOpHs a, Typeable a) => a & a & s :-> a & a & s
assertNotIssuer = do
  dup
  dip $ do
    dip dup
    assertNeq $ mkMTextUnsafe "issuer is not a user"

-- | Add a user with a particular `WhitelistId`,
-- or implicitly remove by providing `Nothing`
--
-- Only admin
addUser :: forall a. (CompareOpHs a, Typeable a) => Entrypoint (NewUserParams a) (Storage a)
addUser = do
  dip $ do
    unStorage
    unpair
    dip $ do
      unpair
      dip $ do
        assertAdmin
      pair
    unpair
  unNewUserParams
  swap
  dip assertNotIssuer
  pair
  swap
  dip $ do
    unpair
    swap
    addUserWhitelist
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
    userWhitelist

-- | Get the `OutboundWhitelists` of a `WhitelistId` or `Nothing` if it's not present
getWhitelist :: forall a. () => Entrypoint (View WhitelistId (Maybe OutboundWhitelists)) (Storage a)
getWhitelist =
  view_ $ do
    unpair
    dip $ do
      unStorage
      cdr
      car
    outboundWhitelists

-- | Get the admin `Address` of the contract
getAdmin :: forall a. () => Entrypoint (View_ Address) (Storage a)
getAdmin =
  view_ $ do
    cdr
    unStorage
    cdr
    cdr

