{-# OPTIONS -Wno-partial-fields -Wno-orphans #-}

module Lorentz.Contracts.Filterlist.CmdLnArgs where

import Control.Applicative
import Text.Show (Show(..))
import Data.List
import Data.Either
import Data.Function (id)
import Data.Functor
import Prelude (FilePath, IO, Ord(..))
import Data.String (IsString(..), String)
import Data.Maybe
import Data.Typeable
import Text.Read

import Lorentz hiding (get)
import Michelson.Parser
import Michelson.Typed.Haskell.Value
import Michelson.Typed.Scope
import Michelson.Typed.Sing
import Michelson.Typed.T
import Util.IO

import qualified Options.Applicative as Opt
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import Data.Constraint
import Data.Singletons
import qualified Data.Set as Set

import Michelson.Typed.Scope.Missing
import Michelson.Typed.Value.Missing ()
import Lorentz.Contracts.Filterlist.Parsers
import Lorentz.Contracts.SomeContractParam
import Lorentz.Contracts.SomeContractStorage
import qualified Lorentz.Contracts.Filterlist as Filterlist
import qualified Lorentz.Contracts.Filterlist.Types as Filterlist
import Lorentz.Contracts.Filterlist.Types
  ( View_
  , FilterlistId
  , FilterlistOutboundParams(..)
  )
import qualified Lorentz.Contracts.Filterlist.Wrapper as Wrapper

assertIsAddress ::
     forall t. Typeable t
  => Sing t
  -> Dict (t ~ ToT Address)
assertIsAddress st =
  case eqT @t @(ToT Address) of
    Nothing ->
      error . fromString $
      unwords
        [ "assertIsAddress:"
        , show (fromSing st)
        , "is not"
        , show (fromSing (sing @(ToT Address)))
        ]
    Just Refl -> Dict

instance SingI ct => Ord (Value ('Tc ct)) where
  compare =
    case sing @ct of
      SCInt -> Prelude.compare
      SCNat -> Prelude.compare
      SCString -> Prelude.compare
      SCBytes -> Prelude.compare
      SCMutez -> Prelude.compare
      SCBool -> Prelude.compare
      SCKeyHash -> Prelude.compare
      SCTimestamp -> Prelude.compare
      SCAddress -> Prelude.compare

-- | Assert `IsComparable`
assertIsComparable ::
     forall (t :: T) a. SingI t
  => (( IsComparable (Value t)
      , SingI (ToCT (Value t))
      , Typeable (ToCT (Value t))
      ) =>
        a)
  -> a
assertIsComparable f =
  case sing @t of
    STc _ -> f
    _ -> error . fromString $
      unwords ["assertIsComparable:", show (fromSing (sing @t)), "is not of the form: TC _"]

-- | Some `Filterlist.Storage` with a `KnownValue` constraint
data SomeStorage where
  SomeStorage :: (KnownValue (Value a))
    => Filterlist.Storage (Value a)
    -> SomeStorage

-- | Run `SomeStorage`
fromSomeStorage :: forall b. SomeStorage -> (forall a. (KnownValue (Value a)) => Filterlist.Storage (Value a) -> b) -> b
fromSomeStorage (SomeStorage xs) f = f xs

-- | Some `Filterlist.TransferParams` with a `NicePrintedValue` constraint
data SomeTransferParams where
  SomeTransferParams :: (NicePrintedValue (Value a))
    => Filterlist.TransferParams (Value a)
    -> SomeTransferParams

-- | Run `SomeTransferParams`
fromSomeTransferParams ::
     forall b.
     SomeTransferParams
  -> (forall a. (NicePrintedValue (Value a)) => Filterlist.TransferParams (Value a) -> b)
  -> b
fromSomeTransferParams (SomeTransferParams xs) f = f xs

data CmdLnArgs
  = Print (SomeSing T) (Maybe FilePath) Bool
  | Init
      { initialStorage :: !SomeStorage
      , initialWrappedStorage :: !(Maybe SomeContractStorage)
      }
  -- | AssertTransfer
  --     { assertTransferParams :: !SomeTransferParams
  --     }
  | AssertReceivers
      { receivers :: ![Address]
      }
  | SetIssuer
      { newIssuer :: !SomeContractParam
      , wrapped :: !Bool
      }
  | UpdateUser
      { newUser :: !SomeContractParam
      , newUserFilterlistId :: !(Maybe FilterlistId)
      , wrapped :: !Bool
      }
  | SetFilterlistOutbound
      { filterlistOutboundParams :: !FilterlistOutboundParams
      , wrapped :: !Bool
      }
  | SetAdmin
      { admin :: !Address
      , wrapped :: !Bool
      }
  | GetIssuer
      { viewIssuer :: !(View_ Address)
      , wrapped :: !Bool
      }
  | GetUser
      { viewUser :: !(View Address (Maybe FilterlistId))
      , wrapped :: !Bool
      }
  | AssertFilterlist
      { assertFilterlist :: !Filterlist.AssertFilterlistParams
      , wrapped :: !Bool
      }
  | GetAdmin
      { viewAdmin :: !(View_ Address)
      , wrapped :: !Bool
      }
  | WrappedParam
      { wrappedParam :: !SomeContractParam
      }

parseList' :: Read a => Opt.ReadM a -> Opt.ReadM [a]
parseList' _ = Opt.auto

-- | Parse and return `Just` or `Nothing` if it fails
parseMaybe :: Alternative f => f a -> f (Maybe a)
parseMaybe p = fmap Just p <|> pure Nothing

-- | Parse `Filterlist.Storage`
parseStorage :: forall a. (Ord a, Read a) => (String -> Opt.Parser a) -> Opt.Parser (Filterlist.Storage a)
parseStorage p =
  (Filterlist.Storage <$>
    p "issuer" <*>
    parseUsers <*>
    parseFilterlists <*>
    parseAddress "admin"
  )
  where
    parseUsers :: Opt.Parser (Filterlist.Users a)
    parseUsers = fmap Filterlist.mkUsers $
      Opt.option (parseList' Opt.auto) $
        mconcat
         [ Opt.long "users"
         , Opt.metavar "Map USER (Maybe FilterlistId)"
         , Opt.help $ "User Filterlists: [(User, Filterlist ID) or Nothing]"
         ]

    parseFilterlists :: Opt.Parser (Filterlist.Filterlists)
    parseFilterlists = fmap Filterlist.mkFilterlists $
      Opt.option
        (parseList' $
          tripleToDouble <$> Opt.auto
        ) $
        mconcat
         [ Opt.long "filterlists"
         , Opt.metavar "Filterlists and their allowed outbound Filterlists"
         , Opt.help $ "Filterlists: Filterlist ID, Restricted, Allowed outbound Filterlist IDs"
         ]
      where
        tripleToDouble :: forall a' b c. (a', b, c) -> (a', (b, c))
        tripleToDouble ~(x, y, z) = (x, (y, z))

-- | Parse `SomeStorage`, see `parseSomeContractParam`
parseSomeStorage :: String -> Opt.Parser SomeStorage
parseSomeStorage name =
  (\(SomeSing (st :: Sing t)) someStorage' ->
    withDict (singIT st) $
    withDict (singTypeableT st) $
    assertOpAbsense @t $
    assertBigMapAbsense @t $
    assertIsComparable @t $
    let parseTypeCheck str =
          either (error . T.pack . show) id $
          parseNoEnv
            (parseTypeCheckValue @t)
            name
            str
    in SomeStorage $ Filterlist.mapStorage (parseTypeCheck . T.pack) someStorage'
  ) <$>
  parseSomeT name <*>
  parseStorage parseString

-- | Parse `SomeTransferParams`, see `parseSomeContractParam`
parseSomeTransferParams :: Opt.Parser SomeTransferParams
parseSomeTransferParams =
  (\(SomeSing (st :: Sing t)) fromStr toStr ->
    withDict (singIT st) $
    withDict (singTypeableT st) $
    assertOpAbsense @t $
    assertBigMapAbsense @t $
    let (parsedFrom, parsedTo) = ( parseNoEnv
                                     (parseTypeCheckValue @t)
                                     name
                                     $ T.pack fromStr
                                 , parseNoEnv
                                     (parseTypeCheckValue @t)
                                     name
                                     $ T.pack toStr
                                 )
     in let (fromVal', toVal') = ( either
                                   (error . T.pack . show)
                                   id
                                   parsedFrom
                               , either
                                   (error . T.pack . show)
                                   id
                                   parsedTo
                               )
     in SomeTransferParams $
        Filterlist.TransferParams fromVal' [toVal'] -- param (st, NStar) (Dict, Dict)
  ) <$>
  parseSomeT "user" <*>
  parseString "from" <*>
  parseString "to"
  where
    name :: IsString str => str
    name = "FilterlistContract"

argParser :: Opt.Parser CmdLnArgs
argParser = Opt.hsubparser $ mconcat
  [ printSubCmd
  , initSubCmd
  -- , assertTransferSubCmd
  -- , assertReceiverSubCmd
  , assertReceiversSubCmd
  , setIssuerSubCmd
  , updateUserSubCmd
  , setFilterlistOutboundSubCmd
  , setAdminSubCmd
  , getIssuerSubCmd
  , getUserSubCmd
  , assertFilterlistSubCmd
  , getAdminSubCmd
  , wrappedParamSubCmd
  ]
  where
    mkCommandParser commandName parser desc =
      Opt.command commandName $
      Opt.info (Opt.helper <*> parser) $
      Opt.progDesc desc

    printSubCmd =
      mkCommandParser "print"
      (Print <$> parseSomeT "value" <*> outputOptions <*> onelineOption)
      "Dump the Filterlist contract in form of Michelson code"

    initSubCmd =
      mkCommandParser "init"
      (Init <$>
        parseSomeStorage "initialStorage" <*>
        parseMaybe (parseSomeContractStorage "initialWrappedStorage")
      )
      ("Initial storage for the (wrapped) Filterlist contract: " <>
      "pass 'initialWrappedStorage' for the wrapped version")

    -- assertTransferSubCmd =
    --   mkCommandParser "AssertTransfer"
    --   (AssertTransfer <$> parseSomeTransferParams)
    --   "Generate the parameter for the Filterlist contract: AssertTransfer"

    -- assertReceiverSubCmd =
    --   mkCommandParser "AssertReceiver"
    --   (AssertReceiver <$> parseAddress "receiver")
    --   "Generate the parameter for the Filterlist contract: AssertReceiver"

    assertReceiversSubCmd =
      mkCommandParser "AssertReceivers"
      (AssertReceivers <$>
        Opt.option
          (parseList' Opt.auto)
          (mconcat
            [ Opt.long "receivers"
            , Opt.metavar "[Address]"
            , Opt.help $ "Assert that all users are filterlisted and " <>
                "not blocklisted, or the issuer"
            ]
          )
      )
      "Generate the parameter for the Filterlist contract: AssertReceivers"

    setIssuerSubCmd =
      mkCommandParser "SetIssuer"
      (SetIssuer <$>
        parseSomeContractParam "newIssuer" <*>
        parseBool "wrapped"
      )
      "Generate the (wrapped) parameter for the Filterlist contract: SetIssuer"

    updateUserSubCmd =
      mkCommandParser "UpdateUser"
      (UpdateUser <$>
        parseSomeContractParam "newUser" <*>
        parseMaybe (parseNatural "newUserFilterlistId") <*>
        parseBool "wrapped"
      )
      "Generate the (wrapped) parameter for the Filterlist contract: UpdateUser"

    setFilterlistOutboundSubCmd =
      mkCommandParser "SetFilterlistOutbound"
      (SetFilterlistOutbound <$>
        (FilterlistOutboundParams <$>
          parseNatural "filterlistId" <*>
          (parseMaybe (Filterlist.mkOutboundFilterlists <$>
            parseBool "restricted" <*>
            Opt.option
              (parseList' Opt.auto)
              (mconcat
                [ Opt.long "outboundFilterlist"
                , Opt.metavar "[Natural]"
                , Opt.help "List of allowed outbound filterlists"
                ]
              )
          ))
        ) <*>
        parseBool "wrapped"
      )
      "Generate the (wrapped) parameter for the Filterlist contract: SetFilterlistOutbound"

    setAdminSubCmd =
      mkCommandParser "SetAdmin"
      (SetAdmin <$>
        parseAddress "admin" <*>
        parseBool "wrapped"
      )
      "Generate the (wrapped) parameter for the Filterlist contract: SetAdmin"

    getIssuerSubCmd =
      mkCommandParser "GetIssuer"
      (GetIssuer <$>
        parseView_ (Proxy @Address) <*>
        parseBool "wrapped"
      )
      "Generate the (wrapped) parameter for the Filterlist contract: GetIssuer"

    getUserSubCmd =
      mkCommandParser "GetUser"
      (GetUser <$>
        parseView @Address @(Maybe Filterlist.FilterlistId) (parseAddress "user") <*> -- (parseSomeContractParam "user")
        parseBool "wrapped"
      )
      "Generate the (wrapped) parameter for the Filterlist contract: GetUser"

    assertFilterlistSubCmd =
      mkCommandParser "AssertFilterlist"
      (AssertFilterlist <$>
        (Filterlist.AssertFilterlistParams <$>
          parseNatural "filterlistId" <*>
          parseMaybe (Filterlist.OutboundFilterlists <$>
            parseBool "unrestricted" <*>
            (Set.fromList <$> parseNaturals "allowedFilterlists")
        )) <*>
        parseBool "wrapped"
      )
      "Generate the (wrapped) parameter for the Filterlist contract: AssertFilterlist"

    getAdminSubCmd =
      mkCommandParser "GetAdmin"
      (GetAdmin <$>
        parseView_ (Proxy @Address) <*>
        parseBool "wrapped"
      )
      "Generate the (wrapped) parameter for the Filterlist contract: GetAdmin"

    wrappedParamSubCmd =
      mkCommandParser "WrappedParam"
      (WrappedParam <$> parseSomeContractParam "wrappedParam")
      ("Generate a wrapped parameter for the Filterlist contract, given the " <>
      "original contract's parameter")

infoMod :: Opt.InfoMod CmdLnArgs
infoMod = mconcat
  [ Opt.fullDesc
  , Opt.progDesc "Filterlist contract CLI interface"
  ]

runCmdLnArgs :: CmdLnArgs -> IO ()
runCmdLnArgs = \case
  Print (SomeSing (st :: Sing t)) mOutput forceOneLine ->
    withDict (singIT st) $
    withDict (singTypeableT st) $
    withDict (assertIsAddress st) $
    -- assertOpAbsense @t $
    -- assertBigMapAbsense @t $
    -- assertIsComparable @t $
    -- withDict (compareOpCT @(ToCT (Value t))) $
    maybe TL.putStrLn writeFileUtf8 mOutput $
    printLorentzContract forceOneLine (Filterlist.filterlistContract @Address) -- (Value t))
  Init {..} ->
    fromSomeStorage initialStorage $ \(initialStorage' :: Filterlist.Storage (Value s)) ->
      assertIsComparable @s $
      case initialWrappedStorage of
        Nothing ->
          TL.putStrLn . printLorentzValue @(Filterlist.Storage (Value s)) forceSingleLine $
          initialStorage'
        Just initialWrappedStorage' ->
          fromSomeContractStorage initialWrappedStorage' $ \(initialWrappedStorage'' :: Value t) ->
          let st = sing @t in
          withDict (singIT st) $
          withDict (singTypeableT st) $
          TL.putStrLn $
          printLorentzValue @(Wrapper.Storage (Value t) (Value s)) forceSingleLine $
          Wrapper.Storage
            initialWrappedStorage''
            initialStorage'
  -- AssertTransfer {..} ->
  --   fromSomeTransferParams assertTransferParams $ \(assertTransferParams' :: Filterlist.TransferParams (Value t)) ->
  --   TL.putStrLn . printLorentzValue forceSingleLine $
  --   Filterlist.AssertTransfer assertTransferParams'
  AssertReceivers {..} ->
    TL.putStrLn . printLorentzValue forceSingleLine $
    Filterlist.AssertReceivers receivers
  SetIssuer {..} ->
    fromSomeContractParam newIssuer $ \(newIssuer' :: Value t) ->
      let st = sing @t in
      withDict (singIT st) $
      withDict (singTypeableT st) $
      if wrapped
         then
           TL.putStrLn . printLorentzValue @(Wrapper.Parameter () (Value t)) forceSingleLine $
           Wrapper.FilterlistParameter $
           Filterlist.SetIssuer newIssuer'
         else
           TL.putStrLn . printLorentzValue @(Filterlist.Parameter (Value t)) forceSingleLine $
           Filterlist.OtherParameter $
           Filterlist.SetIssuer newIssuer'
  UpdateUser {..} ->
    fromSomeContractParam newUser $ \(newUser' :: Value t) ->
      let st = sing @t in
      withDict (singIT st) $
      withDict (singTypeableT st) $
      if wrapped
         then
           TL.putStrLn . printLorentzValue @(Wrapper.Parameter () (Value t)) forceSingleLine $
           Wrapper.FilterlistParameter $
           Filterlist.UpdateUser $
           Filterlist.UpdateUserParams newUser' newUserFilterlistId
         else
           TL.putStrLn . printLorentzValue @(Filterlist.Parameter (Value t)) forceSingleLine $
           Filterlist.OtherParameter $
           Filterlist.UpdateUser $
           Filterlist.UpdateUserParams newUser' newUserFilterlistId
  SetFilterlistOutbound {..} ->
    if wrapped
       then
         TL.putStrLn . printLorentzValue @(Wrapper.Parameter () ()) forceSingleLine $
         Wrapper.FilterlistParameter $
         Filterlist.SetFilterlistOutbound filterlistOutboundParams
       else
         TL.putStrLn . printLorentzValue @(Filterlist.Parameter ()) forceSingleLine $
         Filterlist.OtherParameter $
         Filterlist.SetFilterlistOutbound filterlistOutboundParams
  SetAdmin {..} ->
    if wrapped
       then
         TL.putStrLn . printLorentzValue @(Wrapper.Parameter () ()) forceSingleLine $
         Wrapper.FilterlistParameter $
         Filterlist.SetAdmin admin
       else
         TL.putStrLn . printLorentzValue @(Filterlist.Parameter ()) forceSingleLine $
         Filterlist.OtherParameter $
         Filterlist.SetAdmin admin
  GetIssuer {..} ->
    if wrapped
       then
         TL.putStrLn . printLorentzValue @(Wrapper.Parameter () Address) forceSingleLine $
         Wrapper.FilterlistParameter $
         Filterlist.GetIssuer viewIssuer
       else
         TL.putStrLn . printLorentzValue @(Filterlist.Parameter Address) forceSingleLine $
         Filterlist.OtherParameter $
         Filterlist.GetIssuer viewIssuer
  GetUser {..} ->
    case viewUser of
      View viewParam addr' ->
        -- fromSomeContractParam viewParam $ \(viewParam' :: Value t) ->
          let viewUser' = View viewParam addr'
           in if wrapped
                 then
                   TL.putStrLn . printLorentzValue @(Wrapper.Parameter () Address) forceSingleLine $
                   Wrapper.FilterlistParameter $
                   Filterlist.GetUser viewUser'
                 else
                   TL.putStrLn . printLorentzValue @(Filterlist.Parameter Address) forceSingleLine $
                   Filterlist.OtherParameter $
                   Filterlist.GetUser viewUser'
  AssertFilterlist {..} ->
    if wrapped
       then
         TL.putStrLn . printLorentzValue @(Wrapper.Parameter () ()) forceSingleLine $
         Wrapper.FilterlistParameter $
         Filterlist.AssertFilterlist assertFilterlist
       else
         TL.putStrLn . printLorentzValue @(Filterlist.Parameter ()) forceSingleLine $
         Filterlist.OtherParameter $
         Filterlist.AssertFilterlist assertFilterlist
  GetAdmin {..} ->
    if wrapped
       then
         TL.putStrLn . printLorentzValue @(Wrapper.Parameter () ()) forceSingleLine $
         Wrapper.FilterlistParameter $
         Filterlist.GetAdmin viewAdmin
       else
         TL.putStrLn . printLorentzValue @(Filterlist.Parameter ()) forceSingleLine $
         Filterlist.OtherParameter $
         Filterlist.GetAdmin viewAdmin
  WrappedParam {..} ->
    fromSomeContractParam wrappedParam $ \(wrappedParam' :: Value t) ->
      let st = sing @t in
      withDict (singIT st) $
      withDict (singTypeableT st) $
      TL.putStrLn . printLorentzValue @(Wrapper.Parameter (Value t) ()) forceSingleLine $
      Wrapper.WrappedParameter wrappedParam'
  where
    forceSingleLine = True

