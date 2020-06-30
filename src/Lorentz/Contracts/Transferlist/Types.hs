{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS -Wno-unused-do-bind #-}

module Lorentz.Contracts.Transferlist.Types where

import Prelude hiding ((>>), drop, swap, get)
import GHC.Generics (Generic, Generic1)
import Text.Show (Show(..))
import Text.Read (Read(..))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Lorentz
import Michelson.Text
import Michelson.Typed.Haskell.Value (IsComparable)

-- | A transfer from one user to zero or more users
data TransferParams a = TransferParams
  { -- | The user sending "tokens"
    from :: !a
    -- | The users receiving "tokens"
  , tos  :: ![a]
  }
  deriving (Generic, Generic1)

deriving instance Read a => Read (TransferParams a)

deriving instance Show a => Show (TransferParams a)

deriving instance IsoValue a => IsoValue (TransferParams a)

instance HasTypeAnn a => HasTypeAnn (TransferParams a)

-- | Wrap `TransferParams`
toTransferParams :: (a, [a]) & s :-> TransferParams a & s
toTransferParams = forcedCoerce_

-- | Unwrap `TransferParams`
unTransferParams :: TransferParams a & s :-> (a, [a]) & s
unTransferParams = forcedCoerce_

-- | Set the `OutboundTransferlists` for a particular `TransferlistId` (only admin)
data TransferlistOutboundParams = TransferlistOutboundParams
  { -- | The `TransferlistId` to update the `OutboundTransferlists` for
    transferlist             :: !TransferlistId
    -- | The new `OutboundTransferlists` or `Nothing` to delete the transferlist
  , newOutboundTransferlists :: !(Maybe OutboundTransferlists)
  }
  deriving  (Generic, Read, Show, IsoValue)

instance HasTypeAnn TransferlistOutboundParams

-- | Unwrap `TransferlistOutboundParams`
unTransferlistOutboundParams :: ()
  => TransferlistOutboundParams & s :-> TransferlistId & Maybe OutboundTransferlists & s
unTransferlistOutboundParams = do
  forcedCoerce_
  unpair

-- | Update/insert a user's `TransferlistId` or remove them from the `Transferlists`
data UpdateUserParams a = UpdateUserParams
  { -- | The user to update
    user          :: !a
    -- | The new `TransferlistId` for the user,
    -- or `Nothing` to delete the user from the `Transferlists`
  , userTransferlist :: !(Maybe TransferlistId)
  }
  deriving  (Generic, Generic1)

-- | Unwrap `UpdateUserParams`
unUpdateUserParams :: UpdateUserParams a & s :-> a & Maybe TransferlistId & s
unUpdateUserParams = do
  forcedCoerce_
  unpair

deriving instance Read a => Read (UpdateUserParams a)

deriving instance Show a => Show (UpdateUserParams a)

deriving instance IsoValue a => IsoValue (UpdateUserParams a)

instance HasTypeAnn a => HasTypeAnn (UpdateUserParams a)

-- | Parameters to assert that a transferlist exists (or doesn't, if `Nothing`),
-- that the given `OutboundTransferlists` matches its `unrestricted`,
-- and that the given `outboundTransferlists` is a subset of its
-- `outboundTransferlists`
data AssertTransferlistParams = AssertTransferlistParams
  { -- | The user to update
    transferlistId :: !Natural
    -- | The new `TransferlistId` for the user,
  , contents :: !(Maybe OutboundTransferlists)
  }
  deriving (Eq, Read, Show, Generic, IsoValue, HasTypeAnn)

-- | Unwrap `AssertTransferlistParams`
unAssertTransferlistParams :: AssertTransferlistParams & s :-> (Natural, Maybe OutboundTransferlists) & s
unAssertTransferlistParams = forcedCoerce_

-- | Management and `View` parameters
data Parameter' a
  -- | Set a new issuer (only admin)
  = SetIssuer !a
  -- | Add/update/remove a new user (only admin)
  | UpdateUser !(UpdateUserParams a)
  -- | Set the `OutboundTransferlists` for a particular `TransferlistId` (only admin)
  --
  -- Note: If you unset the `OutboundTransferlists` for a `TransferlistId`
  -- that's referenced in `Users`, the reference will remain and
  -- attempting to lookup the transferlist for any user referencing it
  -- will fail.
  | SetTransferlistOutbound !TransferlistOutboundParams
  -- | Set the admin `Address` (only admin)
  | SetAdmin !Address
  -- | Get the issuer
  | GetIssuer !(View_ a)
  -- | Get a user's `TransferlistId`, or `Nothing` if the user is not transferlisted
  | GetUser !(View a (Maybe TransferlistId))
  -- | Get a transferlist's `OutboundTransferlists`,
  -- or `Nothing` if there's no transferlist with that `TransferlistId`
  | AssertTransferlist !AssertTransferlistParams
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

type TransferlistId = Natural

-- | A transferlisted user is associated to exactly one transferlist
type Users a = BigMap a TransferlistId

-- | `fmap` for `Users` (requires `Ord`)
mapUsers :: Ord b => (a -> b) -> Users a -> Users b
mapUsers f = BigMap . Map.mapKeys f . unBigMap

-- | Construct `Users` from a list of assignments from
-- users to `TransferlistId`'s.
--
-- Note: duplicates will be implicitly removed
mkUsers :: Ord a => [(a, TransferlistId)] -> Users a
mkUsers = BigMap . Map.fromList

-- | Outbound permissions on a transferlist.
--
-- If `unrestricted` is `False`, no transfers from the transferlist are allowed.
--
-- Otherwise, members of the transferlist may only transfer to a transferlist
-- in their `allowedTransferlists`.
data OutboundTransferlists = OutboundTransferlists
  { unrestricted        :: !Bool
  , allowedTransferlists :: !(Set TransferlistId)
  }
  deriving (Eq, Generic, Read, Show, IsoValue)

instance HasTypeAnn OutboundTransferlists

-- | Unwrap `OutboundTransferlists`
unOutboundTransferlists :: OutboundTransferlists & s :-> (Bool, Set TransferlistId) & s
unOutboundTransferlists = forcedCoerce_

assertSubset :: (IterOpHs (Set a), IsError err) => err -> Set a & Set a & s :-> s
assertSubset msg = do
  iter $ do
    dip dup
    mem
    assert msg
  drop

assertSubsetOutboundTransferlists :: OutboundTransferlists & OutboundTransferlists & s :-> s
assertSubsetOutboundTransferlists = do
  unOutboundTransferlists
  dip $ do
    unOutboundTransferlists
    unpair
  unpair
  swap
  dip $ assertEq $ mkMTextUnsafe "unequal unrestricted"
  assertSubset $ mkMTextUnsafe "unequal sets"

-- | An assignment from `TransferlistId` to outbound permissions.
type Transferlists = BigMap TransferlistId OutboundTransferlists

-- | Construct `OutboundTransferlists` from `unrestricted` and a
-- list that will be deduplicated into a `Set` of `TransferlistId`'s
mkOutboundTransferlists :: Bool -> [TransferlistId] -> OutboundTransferlists
mkOutboundTransferlists unrestricted' =
  OutboundTransferlists unrestricted' .
  Set.fromList

-- | Construct `Transferlists` from:
-- @
--  [(key, (unrestricted, outbound TransferlistId's))]
-- @
--
-- Note: The latest `TransferlistId` will replace all previous occurrences
mkTransferlists :: [(TransferlistId, (Bool, [TransferlistId]))] -> Transferlists
mkTransferlists =
  BigMap . fmap (uncurry mkOutboundTransferlists) . Map.fromList

-- | Storage for the `transferlistContract`
data Storage a =
  Storage
    { -- | The issuer, who may transfer to anyone and may not be a user
      issuer :: !a
      -- | Each transferlisted user has exactly one `TransferlistId`
    , users :: !(Users a)
      -- | Each `TransferlistId` is associated with `OutboundTransferlists`
    , transferlists :: !Transferlists
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
mkStorage :: a & Users a & Transferlists & Address & s :-> Storage a & s
mkStorage = do
  pair
  dip pair
  pair
  forcedCoerce_

-- | Unwrap `Storage`
unStorage :: Storage a & s :-> ((a, Users a), (Transferlists, Address)) & s
unStorage = forcedCoerce_

-- | Wrap `Storage`
toStorage :: ((a, Users a), (Transferlists, Address)) & s :-> Storage a & s
toStorage = forcedCoerce_

-- | Specialized `update`
updateUserTransferlist :: forall a s. IsComparable a
  => a & Maybe TransferlistId & Users a & s :-> Users a & s
updateUserTransferlist = update @(Users a)

-- | Specialized `get`
getUserTransferlist :: forall a s. IsComparable a
  => a & Users a & s :-> Maybe TransferlistId & s
getUserTransferlist = get @(Users a) -- case niceComparableEvi @a of
                  -- Sub Dict -> get @(Users a) -- \\ niceComparableEvi @a

-- | Assert that the user is on a transferlist
assertUserTransferlist :: (NiceComparable a, IsComparable a) => a & Users a & s :-> TransferlistId & s
assertUserTransferlist = do
  getUserTransferlist
  assertSome $ mkMTextUnsafe "User not on a transferlist"

-- | Specialized `update`
setOutboundTransferlists :: forall s. ()
  => TransferlistId & Maybe OutboundTransferlists & Transferlists & s :-> Transferlists & s
setOutboundTransferlists = update @Transferlists

-- | Specialized `get`
outboundTransferlists :: forall s. ()
  => TransferlistId & Transferlists & s :-> Maybe OutboundTransferlists & s
outboundTransferlists = get @Transferlists

-- | Assert that a `TransferlistId` has associated `OutboundTransferlists`
assertOutboundTransferlists :: TransferlistId & Transferlists & s :-> OutboundTransferlists & s
assertOutboundTransferlists = do
  outboundTransferlists
  assertSome $ mkMTextUnsafe "Transferlist does not exist"

-- | Assert that `OutboundTransferlists` `unrestricted` is `True`
assertUnrestrictedOutboundTransferlists :: OutboundTransferlists & s :-> Set TransferlistId & s
assertUnrestrictedOutboundTransferlists = do
  unOutboundTransferlists
  unpair
  assert $ mkMTextUnsafe "outbound restricted"

-- | Assert that the user is transferlisted and not blocklisted, or the issuer
--
-- @
--  user & issuer & users & transferlists
-- @
assertReceiver :: forall a s. (NiceComparable a, IsComparable a) -- (NiceComparable a) is not actually redundant
  => a & a & Users a & Transferlists & s :-> a & Users a & Transferlists & s
assertReceiver = do
  swap
  dup
  -- issuer & issuer & user & users & transferlists
  stackType @(a & a & a & Users a & Transferlists & s)
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
         assertUserTransferlist
         swap
         dip $ do
           dip dup
           assertOutboundTransferlists
           assertUnrestrictedOutboundTransferlists
           drop
       )

