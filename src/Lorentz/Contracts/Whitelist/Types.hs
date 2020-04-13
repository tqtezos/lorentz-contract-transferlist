{-# LANGUAGE RebindableSyntax #-}

module Lorentz.Contracts.Whitelist.Types where

import Prelude hiding ((>>), drop, swap, get)
import GHC.Generics (Generic, Generic1)
import Text.Show (Show(..))
import Text.Read (Read(..))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Lorentz
import Michelson.Text
import Michelson.Typed.Haskell.Value (IsComparable)

-- | A transfer between users
data TransferParams a = TransferParams
  { -- | The user sending "tokens"
    from :: !a
    -- | The user receiving "tokens"
  , to   :: !a
  }
  deriving (Generic, Generic1)

deriving instance Read a => Read (TransferParams a)

deriving instance Show a => Show (TransferParams a)

deriving instance IsoValue a => IsoValue (TransferParams a)

instance HasTypeAnn a => HasTypeAnn (TransferParams a)

-- | Wrap `TransferParams`
toTransferParams :: (a, a) & s :-> TransferParams a & s
toTransferParams = forcedCoerce_

-- | Unwrap `TransferParams`
unTransferParams :: TransferParams a & s :-> (a, a) & s
unTransferParams = forcedCoerce_

-- | Set the `OutboundWhitelists` for a particular `WhitelistId` (only admin)
data WhitelistOutboundParams = WhitelistOutboundParams
  { -- | The `WhitelistId` to update the `OutboundWhitelists` for
    whitelist             :: !WhitelistId
    -- | The new `OutboundWhitelists` or `Nothing` to delete the whitelist
  , newOutboundWhitelists :: !(Maybe OutboundWhitelists)
  }
  deriving  (Generic, Read, Show, IsoValue)

instance HasTypeAnn WhitelistOutboundParams

-- | Unwrap `WhitelistOutboundParams`
unWhitelistOutboundParams :: ()
  => WhitelistOutboundParams & s :-> WhitelistId & Maybe OutboundWhitelists & s
unWhitelistOutboundParams = do
  forcedCoerce_
  unpair

-- | Update/insert a user's `WhitelistId` or remove them from the `Whitelists`
data UpdateUserParams a = UpdateUserParams
  { -- | The user to update
    user          :: !a
    -- | The new `WhitelistId` for the user,
    -- or `Nothing` to delete the user from the `Whitelists`
  , updateUserWhitelist :: !(Maybe WhitelistId)
  }
  deriving  (Generic, Generic1)

-- | Unwrap `UpdateUserParams`
unUpdateUserParams :: UpdateUserParams a & s :-> a & Maybe WhitelistId & s
unUpdateUserParams = do
  forcedCoerce_
  unpair

deriving instance Read a => Read (UpdateUserParams a)

deriving instance Show a => Show (UpdateUserParams a)

deriving instance IsoValue a => IsoValue (UpdateUserParams a)

instance HasTypeAnn a => HasTypeAnn (UpdateUserParams a)


-- | Management and `View` parameters
data Parameter' a
  -- | Set a new issuer (only admin)
  = SetIssuer !a
  -- | Add a new user,  (only admin)
  | AddUser !(UpdateUserParams a)
  -- | Set the `OutboundWhitelists` for a particular `WhitelistId` (only admin)
  --
  -- Note: If you unset the `OutboundWhitelists` for a `WhitelistId`
  -- that's referenced in `Users`, the reference will remain and
  -- attempting to lookup the whitelist for any user referencing it
  -- will fail.
  | SetWhitelistOutbound !WhitelistOutboundParams
  -- | Set the admin `Address` (only admin)
  | SetAdmin !Address
  -- | Get the issuer
  | GetIssuer !(View_ a)
  -- | Get a user's `WhitelistId`, or `Nothing` if the user is not whitelisted
  | GetUser !(View a (Maybe WhitelistId))
  -- | Get a whitelist's `OutboundWhitelists`,
  -- or `Nothing` if there's no whitelist with that `WhitelistId`
  | GetWhitelist !(View WhitelistId (Maybe OutboundWhitelists))
  -- | Get the admin `Address`
  | GetAdmin !(View_ Address)
  deriving  (Generic)

deriving instance Show a => Show (Parameter' a)

deriving instance IsoValue a => IsoValue (Parameter' a)

instance (HasTypeAnn a, IsoValue a) => ParameterHasEntryPoints (Parameter' a) where
  type ParameterEntryPointsDerivation (Parameter' a) = EpdPlain

instance HasTypeAnn a => HasTypeAnn (Parameter' a)

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
  deriving (Eq, Generic, Read, Show, IsoValue)

instance HasTypeAnn OutboundWhitelists

-- | Unwrap `OutboundWhitelists`
unOutboundWhitelists :: OutboundWhitelists & s :-> (Bool, Set WhitelistId) & s
unOutboundWhitelists = forcedCoerce_

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
  forcedCoerce_

-- | Unwrap `Storage`
unStorage :: Storage a & s :-> ((a, Users a), (Whitelists, Address)) & s
unStorage = forcedCoerce_

-- | Wrap `Storage`
toStorage :: ((a, Users a), (Whitelists, Address)) & s :-> Storage a & s
toStorage = forcedCoerce_

-- | Specialized `update`
addUserWhitelist :: forall a s. IsComparable a
  => a & Maybe WhitelistId & Users a & s :-> Users a & s
addUserWhitelist = update @(Users a)

-- | Specialized `get`
userWhitelist :: forall a s. IsComparable a
  => a & Users a & s :-> Maybe WhitelistId & s
userWhitelist = get @(Users a) -- case niceComparableEvi @a of
                  -- Sub Dict -> get @(Users a) -- \\ niceComparableEvi @a

-- | Assert that the user is on a whitelist
assertUserWhitelist :: (NiceComparable a, IsComparable a) => a & Users a & s :-> WhitelistId & s
assertUserWhitelist = do
  userWhitelist
  assertSome $ mkMTextUnsafe "User not on a whitelist"

-- | Assert that the users are on whitelists
assertUsersWhitelist :: (NiceComparable a, IsComparable a)
  => a & a & Users a & s :-> WhitelistId & WhitelistId & s
assertUsersWhitelist = do
  dip $ do
    dip dup
    assertUserWhitelist
    swap
  assertUserWhitelist

-- | Specialized `update`
setOutboundWhitelists :: forall s. ()
  => WhitelistId & Maybe OutboundWhitelists & Whitelists & s :-> Whitelists & s
setOutboundWhitelists = update @Whitelists

-- | Specialized `get`
outboundWhitelists :: forall s. ()
  => WhitelistId & Whitelists & s :-> Maybe OutboundWhitelists & s
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

-- | Assert that the user is whitelisted and not blacklisted, or the issuer
--
-- @
--  user & issuer & users & whitelists
-- @
assertReceiver :: forall a s. (NiceComparable a, IsComparable a)
  => a & a & Users a & Whitelists & s :-> a & Users a & Whitelists & s
assertReceiver = do
  swap
  dup
  -- issuer & issuer & user & users & whitelists
  stackType @(a & a & a & Users a & Whitelists & s)
  dip $ do
    dip dup
    eq
    if_ -- user is issuer
       drop
       (do
         dip dup
         assertUserWhitelist
         swap
         dip $ do
           dip dup
           assertOutboundWhitelists
           assertUnrestrictedOutboundWhitelists
           drop
       )

