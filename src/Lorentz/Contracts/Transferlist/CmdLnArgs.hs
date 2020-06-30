{-# OPTIONS -Wno-partial-fields -Wno-orphans #-}

module Lorentz.Contracts.Transferlist.CmdLnArgs where

import Control.Applicative
import Data.Traversable
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
import Lorentz.Contracts.Transferlist.Parsers
import Lorentz.Contracts.SomeContractParam
import Lorentz.Contracts.SomeContractStorage
import qualified Lorentz.Contracts.Transferlist as Transferlist
import qualified Lorentz.Contracts.Transferlist.Types as Transferlist
import Lorentz.Contracts.Transferlist.Types
  ( View_
  , TransferlistId
  , TransferlistOutboundParams(..)
  )
import qualified Lorentz.Contracts.Transferlist.Wrapper as Wrapper

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

-- | Some `Transferlist.Storage` with a `KnownValue` constraint
data SomeStorage where
  SomeStorage :: (KnownValue (Value a))
    => Transferlist.Storage (Value a)
    -> SomeStorage

-- | Run `SomeStorage`
fromSomeStorage :: forall b. SomeStorage -> (forall a. (KnownValue (Value a)) => Transferlist.Storage (Value a) -> b) -> b
fromSomeStorage (SomeStorage xs) f = f xs

-- | Some `Transferlist.TransferParams` with a `NicePrintedValue` constraint
data SomeTransferParams where
  SomeTransferParams :: (NicePrintedValue (Value a))
    => [Transferlist.TransferParams (Value a)]
    -> SomeTransferParams

-- | Run `SomeTransferParams`
fromSomeTransferParams ::
     forall b.
     SomeTransferParams
  -> (forall a. (NicePrintedValue (Value a)) => [Transferlist.TransferParams (Value a)] -> b)
  -> b
fromSomeTransferParams (SomeTransferParams xs) f = f xs

data CmdLnArgs
  = Print (SomeSing T) (Maybe FilePath) Bool
  | Init
      { initialStorage :: !SomeStorage
      , initialWrappedStorage :: !(Maybe SomeContractStorage)
      }
  | AssertTransfers
      { assertTransferParams :: !SomeTransferParams
      }
  | AssertReceivers
      { receivers :: ![Address]
      }
  | SetIssuer
      { newIssuer :: !SomeContractParam
      , wrapped :: !Bool
      }
  | UpdateUser
      { newUser :: !SomeContractParam
      , newUserTransferlistId :: !(Maybe TransferlistId)
      , wrapped :: !Bool
      }
  | SetTransferlistOutbound
      { transferlistOutboundParams :: !TransferlistOutboundParams
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
      { viewUser :: !(View Address (Maybe TransferlistId))
      , wrapped :: !Bool
      }
  | AssertTransferlist
      { assertTransferlist :: !Transferlist.AssertTransferlistParams
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

-- | Parse `Transferlist.Storage`
parseStorage :: forall a. (Ord a, Read a) => (String -> Opt.Parser a) -> Opt.Parser (Transferlist.Storage a)
parseStorage p =
  (Transferlist.Storage <$>
    p "issuer" <*>
    parseUsers <*>
    parseTransferlists <*>
    parseAddress "admin"
  )
  where
    parseUsers :: Opt.Parser (Transferlist.Users a)
    parseUsers = fmap Transferlist.mkUsers $
      Opt.option (parseList' Opt.auto) $
        mconcat
         [ Opt.long "users"
         , Opt.metavar "Map USER (Maybe TransferlistId)"
         , Opt.help $ "User Transferlists: [(User, Transferlist ID) or Nothing]"
         ]

    parseTransferlists :: Opt.Parser (Transferlist.Transferlists)
    parseTransferlists = fmap Transferlist.mkTransferlists $
      Opt.option
        (parseList' $
          tripleToDouble <$> Opt.auto
        ) $
        mconcat
         [ Opt.long "transferlists"
         , Opt.metavar "Transferlists and their allowed outbound Transferlists"
         , Opt.help $ "Transferlists: Transferlist ID, Restricted, Allowed outbound Transferlist IDs"
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
    in SomeStorage $ Transferlist.mapStorage (parseTypeCheck . T.pack) someStorage'
  ) <$>
  parseSomeT name <*>
  parseStorage parseString

-- | Parse `SomeTransferParams`, see `parseSomeContractParam`
parseSomeTransferParams :: Opt.Parser SomeTransferParams
parseSomeTransferParams =
  (\(SomeSing (st :: Sing t)) fromStr toStrs ->
    withDict (singIT st) $
    withDict (singTypeableT st) $
    assertOpAbsense @t $
    assertBigMapAbsense @t $
    let (parsedFrom, parsedTo) = ( parseNoEnv
                                     (parseTypeCheckValue @t)
                                     name
                                     $ T.pack fromStr
                                 , traverse (parseNoEnv
                                     (parseTypeCheckValue @t)
                                     name
                                     . T.pack) toStrs
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
       [Transferlist.TransferParams fromVal' toVal'] -- param (st, NStar) (Dict, Dict)
  ) <$>
  parseSomeT "user" <*>
  parseString "from" <*>
  parseStrings "to"
  where
    name :: IsString str => str
    name = "TransferlistContract"

argParser :: Opt.Parser CmdLnArgs
argParser = Opt.hsubparser $ mconcat
  [ printSubCmd
  , initSubCmd
  , assertTransfersSubCmd
  -- , assertReceiverSubCmd
  , assertReceiversSubCmd
  , setIssuerSubCmd
  , updateUserSubCmd
  , setTransferlistOutboundSubCmd
  , setAdminSubCmd
  , getIssuerSubCmd
  , getUserSubCmd
  , assertTransferlistSubCmd
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
      "Dump the Transferlist contract in form of Michelson code"

    initSubCmd =
      mkCommandParser "init"
      (Init <$>
        parseSomeStorage "initialStorage" <*>
        parseMaybe (parseSomeContractStorage "initialWrappedStorage")
      )
      ("Initial storage for the (wrapped) Transferlist contract: " <>
      "pass 'initialWrappedStorage' for the wrapped version")

    assertTransfersSubCmd =
      mkCommandParser "AssertTransfers"
      (AssertTransfers <$> parseSomeTransferParams)
      "Generate the parameter for the Transferlist contract: AssertTransfers"

    -- assertReceiverSubCmd =
    --   mkCommandParser "AssertReceiver"
    --   (AssertReceiver <$> parseAddress "receiver")
    --   "Generate the parameter for the Transferlist contract: AssertReceiver"

    assertReceiversSubCmd =
      mkCommandParser "AssertReceivers"
      (AssertReceivers <$>
        Opt.option
          (parseList' Opt.auto)
          (mconcat
            [ Opt.long "receivers"
            , Opt.metavar "[Address]"
            , Opt.help $ "Assert that all users are transferlisted and " <>
                "not blocklisted, or the issuer"
            ]
          )
      )
      "Generate the parameter for the Transferlist contract: AssertReceivers"

    setIssuerSubCmd =
      mkCommandParser "SetIssuer"
      (SetIssuer <$>
        parseSomeContractParam "newIssuer" <*>
        parseBool "wrapped"
      )
      "Generate the (wrapped) parameter for the Transferlist contract: SetIssuer"

    updateUserSubCmd =
      mkCommandParser "UpdateUser"
      (UpdateUser <$>
        parseSomeContractParam "newUser" <*>
        parseMaybe (parseNatural "newUserTransferlistId") <*>
        parseBool "wrapped"
      )
      "Generate the (wrapped) parameter for the Transferlist contract: UpdateUser"

    setTransferlistOutboundSubCmd =
      mkCommandParser "SetTransferlistOutbound"
      (SetTransferlistOutbound <$>
        (TransferlistOutboundParams <$>
          parseNatural "transferlistId" <*>
          (parseMaybe (Transferlist.mkOutboundTransferlists <$>
            parseBool "restricted" <*>
            Opt.option
              (parseList' Opt.auto)
              (mconcat
                [ Opt.long "outboundTransferlist"
                , Opt.metavar "[Natural]"
                , Opt.help "List of allowed outbound transferlists"
                ]
              )
          ))
        ) <*>
        parseBool "wrapped"
      )
      "Generate the (wrapped) parameter for the Transferlist contract: SetTransferlistOutbound"

    setAdminSubCmd =
      mkCommandParser "SetAdmin"
      (SetAdmin <$>
        parseAddress "admin" <*>
        parseBool "wrapped"
      )
      "Generate the (wrapped) parameter for the Transferlist contract: SetAdmin"

    getIssuerSubCmd =
      mkCommandParser "GetIssuer"
      (GetIssuer <$>
        parseView_ (Proxy @Address) <*>
        parseBool "wrapped"
      )
      "Generate the (wrapped) parameter for the Transferlist contract: GetIssuer"

    getUserSubCmd =
      mkCommandParser "GetUser"
      (GetUser <$>
        parseView @Address @(Maybe Transferlist.TransferlistId) (parseAddress "user") <*> -- (parseSomeContractParam "user")
        parseBool "wrapped"
      )
      "Generate the (wrapped) parameter for the Transferlist contract: GetUser"

    assertTransferlistSubCmd =
      mkCommandParser "AssertTransferlist"
      (AssertTransferlist <$>
        (Transferlist.AssertTransferlistParams <$>
          parseNatural "transferlistId" <*>
          parseMaybe (Transferlist.OutboundTransferlists <$>
            parseBool "unrestricted" <*>
            (Set.fromList <$> parseNaturals "allowedTransferlists")
        )) <*>
        parseBool "wrapped"
      )
      "Generate the (wrapped) parameter for the Transferlist contract: AssertTransferlist"

    getAdminSubCmd =
      mkCommandParser "GetAdmin"
      (GetAdmin <$>
        parseView_ (Proxy @Address) <*>
        parseBool "wrapped"
      )
      "Generate the (wrapped) parameter for the Transferlist contract: GetAdmin"

    wrappedParamSubCmd =
      mkCommandParser "WrappedParam"
      (WrappedParam <$> parseSomeContractParam "wrappedParam")
      ("Generate a wrapped parameter for the Transferlist contract, given the " <>
      "original contract's parameter")

infoMod :: Opt.InfoMod CmdLnArgs
infoMod = mconcat
  [ Opt.fullDesc
  , Opt.progDesc "Transferlist contract CLI interface"
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
    printLorentzContract forceOneLine (Transferlist.transferlistContract @Address) -- (Value t))
  Init {..} ->
    fromSomeStorage initialStorage $ \(initialStorage' :: Transferlist.Storage (Value s)) ->
      assertIsComparable @s $
      case initialWrappedStorage of
        Nothing ->
          TL.putStrLn . printLorentzValue @(Transferlist.Storage (Value s)) forceSingleLine $
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
  AssertTransfers {..} ->
    fromSomeTransferParams assertTransferParams $ \(assertTransferParams' :: [Transferlist.TransferParams (Value t)]) ->
    TL.putStrLn . printLorentzValue forceSingleLine $
    Transferlist.AssertTransfers assertTransferParams'
  AssertReceivers {..} ->
    TL.putStrLn . printLorentzValue forceSingleLine $
    Transferlist.AssertReceivers receivers
  SetIssuer {..} ->
    fromSomeContractParam newIssuer $ \(newIssuer' :: Value t) ->
      let st = sing @t in
      withDict (singIT st) $
      withDict (singTypeableT st) $
      if wrapped
         then
           TL.putStrLn . printLorentzValue @(Wrapper.Parameter () (Value t)) forceSingleLine $
           Wrapper.TransferlistParameter $
           Transferlist.SetIssuer newIssuer'
         else
           TL.putStrLn . printLorentzValue @(Transferlist.Parameter (Value t)) forceSingleLine $
           Transferlist.OtherParameter $
           Transferlist.SetIssuer newIssuer'
  UpdateUser {..} ->
    fromSomeContractParam newUser $ \(newUser' :: Value t) ->
      let st = sing @t in
      withDict (singIT st) $
      withDict (singTypeableT st) $
      if wrapped
         then
           TL.putStrLn . printLorentzValue @(Wrapper.Parameter () (Value t)) forceSingleLine $
           Wrapper.TransferlistParameter $
           Transferlist.UpdateUser $
           Transferlist.UpdateUserParams newUser' newUserTransferlistId
         else
           TL.putStrLn . printLorentzValue @(Transferlist.Parameter (Value t)) forceSingleLine $
           Transferlist.OtherParameter $
           Transferlist.UpdateUser $
           Transferlist.UpdateUserParams newUser' newUserTransferlistId
  SetTransferlistOutbound {..} ->
    if wrapped
       then
         TL.putStrLn . printLorentzValue @(Wrapper.Parameter () ()) forceSingleLine $
         Wrapper.TransferlistParameter $
         Transferlist.SetTransferlistOutbound transferlistOutboundParams
       else
         TL.putStrLn . printLorentzValue @(Transferlist.Parameter ()) forceSingleLine $
         Transferlist.OtherParameter $
         Transferlist.SetTransferlistOutbound transferlistOutboundParams
  SetAdmin {..} ->
    if wrapped
       then
         TL.putStrLn . printLorentzValue @(Wrapper.Parameter () ()) forceSingleLine $
         Wrapper.TransferlistParameter $
         Transferlist.SetAdmin admin
       else
         TL.putStrLn . printLorentzValue @(Transferlist.Parameter ()) forceSingleLine $
         Transferlist.OtherParameter $
         Transferlist.SetAdmin admin
  GetIssuer {..} ->
    if wrapped
       then
         TL.putStrLn . printLorentzValue @(Wrapper.Parameter () Address) forceSingleLine $
         Wrapper.TransferlistParameter $
         Transferlist.GetIssuer viewIssuer
       else
         TL.putStrLn . printLorentzValue @(Transferlist.Parameter Address) forceSingleLine $
         Transferlist.OtherParameter $
         Transferlist.GetIssuer viewIssuer
  GetUser {..} ->
    case viewUser of
      View viewParam addr' ->
        -- fromSomeContractParam viewParam $ \(viewParam' :: Value t) ->
          let viewUser' = View viewParam addr'
           in if wrapped
                 then
                   TL.putStrLn . printLorentzValue @(Wrapper.Parameter () Address) forceSingleLine $
                   Wrapper.TransferlistParameter $
                   Transferlist.GetUser viewUser'
                 else
                   TL.putStrLn . printLorentzValue @(Transferlist.Parameter Address) forceSingleLine $
                   Transferlist.OtherParameter $
                   Transferlist.GetUser viewUser'
  AssertTransferlist {..} ->
    if wrapped
       then
         TL.putStrLn . printLorentzValue @(Wrapper.Parameter () ()) forceSingleLine $
         Wrapper.TransferlistParameter $
         Transferlist.AssertTransferlist assertTransferlist
       else
         TL.putStrLn . printLorentzValue @(Transferlist.Parameter ()) forceSingleLine $
         Transferlist.OtherParameter $
         Transferlist.AssertTransferlist assertTransferlist
  GetAdmin {..} ->
    if wrapped
       then
         TL.putStrLn . printLorentzValue @(Wrapper.Parameter () ()) forceSingleLine $
         Wrapper.TransferlistParameter $
         Transferlist.GetAdmin viewAdmin
       else
         TL.putStrLn . printLorentzValue @(Transferlist.Parameter ()) forceSingleLine $
         Transferlist.OtherParameter $
         Transferlist.GetAdmin viewAdmin
  WrappedParam {..} ->
    fromSomeContractParam wrappedParam $ \(wrappedParam' :: Value t) ->
      let st = sing @t in
      withDict (singIT st) $
      withDict (singTypeableT st) $
      TL.putStrLn . printLorentzValue @(Wrapper.Parameter (Value t) ()) forceSingleLine $
      Wrapper.WrappedParameter wrappedParam'
  where
    forceSingleLine = True

