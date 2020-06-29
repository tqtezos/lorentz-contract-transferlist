{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS -Wno-unused-do-bind #-}

module Lorentz.Contracts.Filterlist.Types where

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

-- | Set the `OutboundFilterlists` for a particular `FilterlistId` (only admin)
data FilterlistOutboundParams = FilterlistOutboundParams
  { -- | The `FilterlistId` to update the `OutboundFilterlists` for
    filterlist             :: !FilterlistId
    -- | The new `OutboundFilterlists` or `Nothing` to delete the filterlist
  , newOutboundFilterlists :: !(Maybe OutboundFilterlists)
  }
  deriving  (Generic, Read, Show, IsoValue)

instance HasTypeAnn FilterlistOutboundParams

-- | Unwrap `FilterlistOutboundParams`
unFilterlistOutboundParams :: ()
  => FilterlistOutboundParams & s :-> FilterlistId & Maybe OutboundFilterlists & s
unFilterlistOutboundParams = do
  forcedCoerce_
  unpair

-- | Update/insert a user's `FilterlistId` or remove them from the `Filterlists`
data UpdateUserParams a = UpdateUserParams
  { -- | The user to update
    user          :: !a
    -- | The new `FilterlistId` for the user,
    -- or `Nothing` to delete the user from the `Filterlists`
  , userFilterlist :: !(Maybe FilterlistId)
  }
  deriving  (Generic, Generic1)

-- | Unwrap `UpdateUserParams`
unUpdateUserParams :: UpdateUserParams a & s :-> a & Maybe FilterlistId & s
unUpdateUserParams = do
  forcedCoerce_
  unpair

deriving instance Read a => Read (UpdateUserParams a)

deriving instance Show a => Show (UpdateUserParams a)

deriving instance IsoValue a => IsoValue (UpdateUserParams a)

instance HasTypeAnn a => HasTypeAnn (UpdateUserParams a)


-- data FilterlistContents = FilterlistContents
--   { unrestricted       :: !Bool
--   , allowedFilterlists :: !(Set Natural)
--   }
--   deriving (Eq, Ord, Read, Show, Generic, IsoValue, HasTypeAnn)
data AssertFilterlistParams = AssertFilterlistParams
  { -- | The user to update
    filterlistId :: !Natural
    -- | The new `FilterlistId` for the user,
  , contents :: !(Maybe OutboundFilterlists)
  }
  deriving (Eq, Read, Show, Generic, IsoValue, HasTypeAnn)

-- | Unwrap `AssertFilterlistParams`
unAssertFilterlistParams :: AssertFilterlistParams & s :-> (Natural, Maybe OutboundFilterlists) & s
unAssertFilterlistParams = forcedCoerce_

-- | Management and `View` parameters
data Parameter' a
  -- | Set a new issuer (only admin)
  = SetIssuer !a
  -- | Add/update/remove a new user (only admin)
  | UpdateUser !(UpdateUserParams a)
  -- | Set the `OutboundFilterlists` for a particular `FilterlistId` (only admin)
  --
  -- Note: If you unset the `OutboundFilterlists` for a `FilterlistId`
  -- that's referenced in `Users`, the reference will remain and
  -- attempting to lookup the filterlist for any user referencing it
  -- will fail.
  | SetFilterlistOutbound !FilterlistOutboundParams
  -- | Set the admin `Address` (only admin)
  | SetAdmin !Address
  -- | Get the issuer
  | GetIssuer !(View_ a)
  -- | Get a user's `FilterlistId`, or `Nothing` if the user is not filterlisted
  | GetUser !(View a (Maybe FilterlistId))
  -- | Get a filterlist's `OutboundFilterlists`,
  -- or `Nothing` if there's no filterlist with that `FilterlistId`
  | AssertFilterlist !AssertFilterlistParams
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

type FilterlistId = Natural

-- | A filterlisted user is associated to exactly one filterlist
type Users a = BigMap a FilterlistId

-- | `fmap` for `Users` (requires `Ord`)
mapUsers :: Ord b => (a -> b) -> Users a -> Users b
mapUsers f = BigMap . Map.mapKeys f . unBigMap

-- | Construct `Users` from a list of assignments from
-- users to `FilterlistId`'s.
--
-- Note: duplicates will be implicitly removed
mkUsers :: Ord a => [(a, FilterlistId)] -> Users a
mkUsers = BigMap . Map.fromList

-- | Outbound permissions on a filterlist.
--
-- If `unrestricted` is `False`, no transfers from the filterlist are allowed.
--
-- Otherwise, members of the filterlist may only transfer to a filterlist
-- in their `allowedFilterlists`.
data OutboundFilterlists = OutboundFilterlists
  { unrestricted        :: !Bool
  , allowedFilterlists :: !(Set FilterlistId)
  }
  deriving (Eq, Generic, Read, Show, IsoValue)

instance HasTypeAnn OutboundFilterlists

-- | Unwrap `OutboundFilterlists`
unOutboundFilterlists :: OutboundFilterlists & s :-> (Bool, Set FilterlistId) & s
unOutboundFilterlists = forcedCoerce_

assertSubset :: (IterOpHs (Set a), IsError err) => err -> Set a & Set a & s :-> s
assertSubset msg = do
  iter $ do
    dip dup
    mem
    assert msg
  drop

assertSubsetOutboundFilterlists :: OutboundFilterlists & OutboundFilterlists & s :-> s
assertSubsetOutboundFilterlists = do
  unOutboundFilterlists
  dip $ do
    unOutboundFilterlists
    unpair
  unpair
  swap
  dip $ assertEq $ mkMTextUnsafe "unequal unrestricted"
  assertSubset $ mkMTextUnsafe "unequal sets"

-- | An assignment from `FilterlistId` to outbound permissions.
type Filterlists = BigMap FilterlistId OutboundFilterlists

-- | Construct `OutboundFilterlists` from `unrestricted` and a
-- list that will be deduplicated into a `Set` of `FilterlistId`'s
mkOutboundFilterlists :: Bool -> [FilterlistId] -> OutboundFilterlists
mkOutboundFilterlists unrestricted' =
  OutboundFilterlists unrestricted' .
  Set.fromList

-- | Construct `Filterlists` from:
-- @
--  [(key, (unrestricted, outbound FilterlistId's))]
-- @
--
-- Note: The latest `FilterlistId` will replace all previous occurrences
mkFilterlists :: [(FilterlistId, (Bool, [FilterlistId]))] -> Filterlists
mkFilterlists =
  BigMap . fmap (uncurry mkOutboundFilterlists) . Map.fromList

-- | Storage for the `filterlistContract`
data Storage a =
  Storage
    { -- | The issuer, who may transfer to anyone and may not be a user
      issuer :: !a
      -- | Each filterlisted user has exactly one `FilterlistId`
    , users :: !(Users a)
      -- | Each `FilterlistId` is associated with `OutboundFilterlists`
    , filterlists :: !Filterlists
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
mkStorage :: a & Users a & Filterlists & Address & s :-> Storage a & s
mkStorage = do
  pair
  dip pair
  pair
  forcedCoerce_

-- | Unwrap `Storage`
unStorage :: Storage a & s :-> ((a, Users a), (Filterlists, Address)) & s
unStorage = forcedCoerce_

-- | Wrap `Storage`
toStorage :: ((a, Users a), (Filterlists, Address)) & s :-> Storage a & s
toStorage = forcedCoerce_

-- | Specialized `update`
updateUserFilterlist :: forall a s. IsComparable a
  => a & Maybe FilterlistId & Users a & s :-> Users a & s
updateUserFilterlist = update @(Users a)

-- | Specialized `get`
getUserFilterlist :: forall a s. IsComparable a
  => a & Users a & s :-> Maybe FilterlistId & s
getUserFilterlist = get @(Users a) -- case niceComparableEvi @a of
                  -- Sub Dict -> get @(Users a) -- \\ niceComparableEvi @a

-- | Assert that the user is on a filterlist
assertUserFilterlist :: (NiceComparable a, IsComparable a) => a & Users a & s :-> FilterlistId & s
assertUserFilterlist = do
  getUserFilterlist
  assertSome $ mkMTextUnsafe "User not on a filterlist"

-- | Specialized `update`
setOutboundFilterlists :: forall s. ()
  => FilterlistId & Maybe OutboundFilterlists & Filterlists & s :-> Filterlists & s
setOutboundFilterlists = update @Filterlists

-- | Specialized `get`
outboundFilterlists :: forall s. ()
  => FilterlistId & Filterlists & s :-> Maybe OutboundFilterlists & s
outboundFilterlists = get @Filterlists

-- | Assert that a `FilterlistId` has associated `OutboundFilterlists`
assertOutboundFilterlists :: FilterlistId & Filterlists & s :-> OutboundFilterlists & s
assertOutboundFilterlists = do
  outboundFilterlists
  assertSome $ mkMTextUnsafe "Filterlist does not exist"

-- | Assert that `OutboundFilterlists` `unrestricted` is `True`
assertUnrestrictedOutboundFilterlists :: OutboundFilterlists & s :-> Set FilterlistId & s
assertUnrestrictedOutboundFilterlists = do
  unOutboundFilterlists
  unpair
  assert $ mkMTextUnsafe "outbound restricted"

-- | Assert that the user is filterlisted and not blocklisted, or the issuer
--
-- @
--  user & issuer & users & filterlists
-- @
assertReceiver :: forall a s. (NiceComparable a, IsComparable a) -- (NiceComparable a) is not actually redundant
  => a & a & Users a & Filterlists & s :-> a & Users a & Filterlists & s
assertReceiver = do
  swap
  dup
  -- issuer & issuer & user & users & filterlists
  stackType @(a & a & a & Users a & Filterlists & s)
  dip $ do
    dip dup
    eq
    if_ -- user is issuer
       (do
         push $ mkMTextUnsafe "issuer not receiver"
         failWith
       )
       (do
         dip dup
         assertUserFilterlist
         swap
         dip $ do
           dip dup
           assertOutboundFilterlists
           assertUnrestrictedOutboundFilterlists
           drop
       )

