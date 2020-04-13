{-# OPTIONS -Wno-missing-export-lists #-}
{-# OPTIONS -Wno-orphans #-}

module Lorentz.Contracts.SomeContractParam where

import Control.Monad
import Michelson.Typed.T
import Prelude (Typeable, ReaderT(..), Either(..), Bool(..), ($), either, flip, (.))
import Text.Show
import qualified Prelude as P

import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Constraint
import Data.Default
import Data.Singletons
import Data.Type.Equality

import Michelson.TypeCheck
import Michelson.Typed

import Michelson.Typed.Scope.Missing


instance ToJSON T where
instance FromJSON T where

-- | `runTypeCheckTest` lifted to `TypeCheckInstr`
runTypeCheckInstr :: TypeCheckInstr a -> Either TCError a
runTypeCheckInstr = runTypeCheckIsolated . flip runReaderT def

-- | A contract parameter with some type
data SomeContractParam where
  SomeContractParam
    :: (SingI t, Typeable t)
    => Value t
    -> (Sing t, Notes t)
    -> (Dict (HasNoOp t), Dict (HasNoBigMap t))
    -> SomeContractParam

-- | `eqType` specialized to allow passing @a, b@ using `Sing`
eqTypeSing ::
     forall (a :: T) (b :: T). P.Each '[ Typeable, SingI] '[ a, b]
  => Sing a
  -> Sing b
  -> Either TCTypeError (a :~: b)
eqTypeSing _ _ = eqType

instance P.Eq SomeContractParam where
  paramX == paramY =
    case paramX of
      SomeContractParam xs (singX, _) (Dict, Dict) ->
        case paramY of
          SomeContractParam ys (singY, _) (Dict, Dict) ->
            case eqTypeSing singX singY of
              Left _ -> False
              Right Refl -> xs P.== ys

instance Show SomeContractParam where
  show (SomeContractParam xs (_, _) (Dict, Dict)) = show xs

-- | Convert an `IsoValue` value to `SomeContractParam`
toSomeContractParam ::
     ( IsoValue t
     , SingI (ToT t)
     , Typeable (ToT t)
     , HasNoOp (ToT t)
     , HasNoBigMap (ToT t)
     )
  => t
  -> SomeContractParam
toSomeContractParam xs =
  SomeContractParam (toVal xs) (sing, starNotes) (Dict, Dict)

-- | Consume `SomeContractParam`
fromSomeContractParam ::
     SomeContractParam
  -> (forall t. (SingI t, Typeable t, HasNoOp t, HasNoBigMap t) =>
                  Value t -> r)
  -> r
fromSomeContractParam (SomeContractParam xs (_, _) (Dict, Dict)) f = f xs


instance ToJSON SomeContractParam where
  toJSON (SomeContractParam xs (sing', _) (Dict, _)) =
    toJSON (fromSing sing', untypeValue xs)

instance FromJSON SomeContractParam where
  parseJSON x = do
    (t, uValue) <- parseJSON x
    withSomeSingT t $ \(singT :: Sing t) ->
      withDict (singIT singT) $ do
        tValue <- -- _ $
          either (fail . show) return . runTypeCheckInstr $
          typeCheckValue @t uValue
        return $
          assertOpAbsense @t $
          assertBigMapAbsense @t $
          SomeContractParam tValue (sing @t, starNotes @t) (Dict, Dict)

