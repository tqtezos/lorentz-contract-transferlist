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

-- | A transfer between users
data TransferParams a = TransferParams
  { -- | The user sending "tokens"
    from :: !a
    -- | The user receiving "tokens"
  , to   :: !a
  }
  deriving  (Generic)
  deriving  (Generic1)

deriving instance Read a => Read (TransferParams a)

deriving instance Show a => Show (TransferParams a)

deriving instance IsoValue a => IsoValue (TransferParams a)

-- | Wrap `TransferParams`
toTransferParams :: (a, a) & s :-> TransferParams a & s
toTransferParams = coerce_

-- | Unwrap `TransferParams`
unTransferParams :: TransferParams a & s :-> (a, a) & s
unTransferParams = coerce_

-- | Set the `OutboundWhitelists` for a particular `WhitelistId` (only admin)
data WhitelistOutboundParams = WhitelistOutboundParams
  { -- | The `WhitelistId` to update the `OutboundWhitelists` for
    whitelist             :: !WhitelistId
    -- | The new `OutboundWhitelists` or `Nothing` to delete the whitelist
  , newOutboundWhitelists :: !(Maybe OutboundWhitelists)
  }
  deriving  (Generic)
  deriving  (Read)
  deriving  (Show)
  deriving  (IsoValue)

-- | Unwrap `WhitelistOutboundParams`
unWhitelistOutboundParams :: WhitelistOutboundParams & s :-> WhitelistId & Maybe OutboundWhitelists & s
unWhitelistOutboundParams = do
  coerce_
  unpair

-- | Update/insert a user's `WhitelistId` or remove them from the `Whitelists`
data UpdateUserParams a = UpdateUserParams
  { -- | The user to update
    user          :: !a
    -- | The new `WhitelistId` for the user,
    -- or `Nothing` to delete the user from the `Whitelists`
  , updateUserWhitelist :: !(Maybe WhitelistId)
  }
  deriving  (Generic)
  deriving  (Generic1)

-- | Unwrap `UpdateUserParams`
unUpdateUserParams :: UpdateUserParams a & s :-> a & Maybe WhitelistId & s
unUpdateUserParams = do
  coerce_
  unpair

deriving instance Read a => Read (UpdateUserParams a)

deriving instance Show a => Show (UpdateUserParams a)

deriving instance IsoValue a => IsoValue (UpdateUserParams a)

-- | Parameters are separated into assertions and management/`View`
-- parameters (`Parameter'`).
--
-- This helps facilitate using the code as different kinds of wrappers
-- while retaining a static type interface for the management/`View`
-- parameters.
data Parameter a
  -- | Assert that a transfer is valid
  = AssertTransfer
      { transferParams :: !(TransferParams a)
      }
  -- | Management and `View` parameters
  | OtherParameter
      { otherParams :: !(Parameter' a)
      }
  deriving  (Generic)
  -- Assert that multiple transfers are valid
  -- AssertTransfers
  --     { transfersParams :: ![TransferParams a]
  --     }

instance NiceParameter a => ParameterEntryPoints (Parameter a) where
  parameterEntryPoints = pepNone

deriving instance (NiceParameter a, Read a) => Read (Parameter a)

deriving instance Show a => Show (Parameter a)

deriving instance IsoValue a => IsoValue (Parameter a)

-- | Management and `View` parameters
data Parameter' a
  -- | Set a new issuer (only admin)
  = SetIssuer
      { newIssuer :: !a
      }
  -- | Add a new user,  (only admin)
  | AddUser
      { newUserParams :: !(UpdateUserParams a)
      }
  -- | Set the `OutboundWhitelists` for a particular `WhitelistId` (only admin)
  --
  -- Note: If you unset the `OutboundWhitelists` for a `WhitelistId`
  -- that's referenced in `Users`, the reference will remain and
  -- attempting to lookup the whitelist for any user referencing it
  -- will fail.
  | SetWhitelistOutbound
      { whitelistOutboundParams :: !WhitelistOutboundParams
      }
  -- | Set the admin `Address` (only admin)
  | SetAdmin
      { admin :: !Address
      }
  -- | Get the issuer
  | GetIssuer
      { viewIssuer :: !(View_ a)
      }
  -- | Get a user's `WhitelistId`, or `Nothing` if the user is not whitelisted
  | GetUser
      { viewUser :: !(View a (Maybe WhitelistId))
      }
  -- | Get a whitelist's `OutboundWhitelists`,
  -- or `Nothing` if there's no whitelist with that `WhitelistId`
  | GetWhitelist
      { viewWhitelist :: !(View WhitelistId (Maybe OutboundWhitelists))
      }
  -- | Get the admin `Address`
  | GetAdmin
      { viewAdmin :: !(View_ Address)
      }
  deriving  (Generic)

deriving instance (NiceParameter a, Read a) => Read (Parameter' a)

deriving instance Show a => Show (Parameter' a)

deriving instance IsoValue a => IsoValue (Parameter' a)

-- | Convenience utility for calling `Parameter`
toAssertTransfer :: forall a s. KnownValue a => TransferParams a & s :-> Parameter a & s
toAssertTransfer = do
  left @(TransferParams a) @(Parameter' a)
  coerce_

-- | An easy way to call an instance of a `whitelistContract`
callAssertTransfer :: forall a s. (NiceParameter a) => TransferParams a & Address & s :-> Operation & s
callAssertTransfer = do
  toAssertTransfer
  dip $ do
    contract @(Parameter a)
    assertSome $ mkMTextUnsafe "Expected Whitelist Contract"
    push (toEnum 0 :: Mutez)
  transferTokens

-- | A parameter-free `View`
type View_ = View ()

type WhitelistId = Natural

-- | A whitelisted user is associated to exactly one whitelist
type Users a = BigMap a WhitelistId

-- | `fmap` for `Users` (requires `Ord`)
mapUsers :: Ord b => (a -> b) -> Users a -> Users b
mapUsers f = BigMap . Map.mapKeys f . unBigMap

-- | Construct `Users` from a list of assignments from
-- users to `WhitelistId`'s.
--
-- Note: duplicates will be implicitly removed
mkUsers :: Ord a => [(a, WhitelistId)] -> Users a
mkUsers = BigMap . Map.fromList

-- | Outbound permissions on a whitelist.
--
-- If `unrestricted` is `False`, no transfers from the whitelist are allowed.
--
-- Otherwise, members of the whitelist may only transfer to a whitelist
-- in their `allowedWhitelists`.
data OutboundWhitelists = OutboundWhitelists
  { unrestricted        :: !Bool
  , allowedWhitelists :: !(Set WhitelistId)
  }
  deriving  (Generic)
  deriving  (Read)
  deriving  (Show)
  deriving  (IsoValue)

-- | Unwrap `OutboundWhitelists`
unOutboundWhitelists :: OutboundWhitelists & s :-> (Bool, Set WhitelistId) & s
unOutboundWhitelists = coerce_

-- | An assignment from `WhitelistId` to outbound permissions.
type Whitelists = BigMap WhitelistId OutboundWhitelists

-- | Construct `OutboundWhitelists` from `unrestricted` and a
-- list that will be deduplicated into a `Set` of `WhitelistId`'s
mkOutboundWhitelists :: Bool -> [WhitelistId] -> OutboundWhitelists
mkOutboundWhitelists unrestricted' =
  OutboundWhitelists unrestricted' .
  Set.fromList

-- | Construct `Whitelists` from:
-- @
--  [(key, (unrestricted, outbound WhitelistId's))]
-- @
--
-- Note: The latest `WhitelistId` will replace all previous occurrences
mkWhitelists :: [(WhitelistId, (Bool, [WhitelistId]))] -> Whitelists
mkWhitelists =
  BigMap . fmap (uncurry mkOutboundWhitelists) . Map.fromList

-- | Storage for the `whitelistContract`
data Storage a =
  Storage
    { -- | The issuer, who may transfer to anyone and may not be a user
      issuer :: !a
      -- | Each whitelisted user has exactly one `WhitelistId`
    , users :: !(Users a)
      -- | Each `WhitelistId` is associated with `OutboundWhitelists`
    , whitelists :: !Whitelists
      -- | The admin is not distinguished as a user.
      --
      -- Entrypoints can update state iff they are admin-only.
    , admin :: !Address
    }
  deriving  (Generic)

deriving instance Show a => Show (Storage a)

deriving instance (Ord a, IsoValue a, IsoCValue a) => IsoValue (Storage a)

-- | `fmap` for `Storage` (requires `Ord`)
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

-- | A contract that accepts either one or a batch of `TransferParams`
-- and throws and error if any transfers are disallowed.
--
-- Only whitelisted users and the "issuer" may participate in transfers.
-- - The issuer may transfer to any whitelisted user
-- - Each whitelisted user is associated to exactly one whitelist, by `WhitelistId`
-- - Each whitelist has a set of outbound whitelists that it may transfer to
-- - A whitelist may be restricted, i.e. its set of outbound whitelists is "empty"
--  * We store both @(`unrestricted` :: `Bool`)@ and
--    a set of `WhitelistId`'s so that restriction may be easily toggled
-- - A user may transfer to any other whitelisted user with a whitelist in their whitelist's outbound list
--
-- See `Parameter` and `Storage` for more detail.
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

-- | Assert that one user is allowed to transfer to the other.
--
-- The `issuer` is allowed to transfer to anyone.
--
-- If the sender's `WhitelistId`'s `OutboundWhitelists` is `unrestricted`,
-- they may transfer to any receiver whose `WhitelistId` is in their
-- `allowedWhitelists`.
assertTransfer ::
     forall a s. (IsComparable a, CompareOpHs a, Typeable a)
  => TransferParams a & Storage a & s :-> ([Operation], Storage a) & s
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
addUser :: forall a. (CompareOpHs a, Typeable a) => Entrypoint (UpdateUserParams a) (Storage a)
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
  unUpdateUserParams
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

