{-# OPTIONS -Wno-partial-fields -Wno-orphans #-}

module Lorentz.Contracts.Whitelist.CmdLnArgs where

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
import Lorentz.Contracts.Whitelist.Parsers
import Lorentz.Contracts.SomeContractParam
import Lorentz.Contracts.SomeContractStorage
import qualified Lorentz.Contracts.Whitelist as Whitelist
import qualified Lorentz.Contracts.Whitelist.Types as Whitelist
import Lorentz.Contracts.Whitelist.Types
  ( View_
  , WhitelistId
  , WhitelistOutboundParams(..)
  )
import qualified Lorentz.Contracts.Whitelist.Wrapper as Wrapper

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

-- | Some `Whitelist.Storage` with a `KnownValue` constraint
data SomeStorage where
  SomeStorage :: (KnownValue (Value a))
    => Whitelist.Storage (Value a)
    -> SomeStorage

-- | Run `SomeStorage`
fromSomeStorage :: forall b. SomeStorage -> (forall a. (KnownValue (Value a)) => Whitelist.Storage (Value a) -> b) -> b
fromSomeStorage (SomeStorage xs) f = f xs

-- | Some `Whitelist.TransferParams` with a `NicePrintedValue` constraint
data SomeTransferParams where
  SomeTransferParams :: (NicePrintedValue (Value a))
    => Whitelist.TransferParams (Value a)
    -> SomeTransferParams

-- | Run `SomeTransferParams`
fromSomeTransferParams ::
     forall b.
     SomeTransferParams
  -> (forall a. (NicePrintedValue (Value a)) => Whitelist.TransferParams (Value a) -> b)
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
      , newUserWhitelistId :: !(Maybe WhitelistId)
      , wrapped :: !Bool
      }
  | SetWhitelistOutbound
      { whitelistOutboundParams :: !WhitelistOutboundParams
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
      { viewUser :: !(View Address (Maybe WhitelistId))
      , wrapped :: !Bool
      }
  | AssertFilterlist
      { assertFilterlist :: !Whitelist.AssertFilterlistParams
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

-- | Parse `Whitelist.Storage`
parseStorage :: forall a. (Ord a, Read a) => (String -> Opt.Parser a) -> Opt.Parser (Whitelist.Storage a)
parseStorage p =
  (Whitelist.Storage <$>
    p "issuer" <*>
    parseUsers <*>
    parseWhitelists <*>
    parseAddress "admin"
  )
  where
    parseUsers :: Opt.Parser (Whitelist.Users a)
    parseUsers = fmap Whitelist.mkUsers $
      Opt.option (parseList' Opt.auto) $
        mconcat
         [ Opt.long "users"
         , Opt.metavar "Map USER (Maybe WhitelistId)"
         , Opt.help $ "User Whitelists: [(User, Whitelist ID) or Nothing]"
         ]

    parseWhitelists :: Opt.Parser (Whitelist.Whitelists)
    parseWhitelists = fmap Whitelist.mkWhitelists $
      Opt.option
        (parseList' $
          tripleToDouble <$> Opt.auto
        ) $
        mconcat
         [ Opt.long "whitelists"
         , Opt.metavar "Whitelists and their allowed outbound Whitelists"
         , Opt.help $ "Whitelists: Whitelist ID, Restricted, Allowed outbound Whitelist IDs"
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
    in SomeStorage $ Whitelist.mapStorage (parseTypeCheck . T.pack) someStorage'
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
        Whitelist.TransferParams fromVal' toVal' -- param (st, NStar) (Dict, Dict)
  ) <$>
  parseSomeT "user" <*>
  parseString "from" <*>
  parseString "to"
  where
    name :: IsString str => str
    name = "WhitelistContract"

argParser :: Opt.Parser CmdLnArgs
argParser = Opt.hsubparser $ mconcat
  [ printSubCmd
  , initSubCmd
  -- , assertTransferSubCmd
  -- , assertReceiverSubCmd
  , assertReceiversSubCmd
  , setIssuerSubCmd
  , updateUserSubCmd
  , setWhitelistOutboundSubCmd
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
      "Dump the Whitelist contract in form of Michelson code"

    initSubCmd =
      mkCommandParser "init"
      (Init <$>
        parseSomeStorage "initialStorage" <*>
        parseMaybe (parseSomeContractStorage "initialWrappedStorage")
      )
      ("Initial storage for the (wrapped) Whitelist contract: " <>
      "pass 'initialWrappedStorage' for the wrapped version")

    -- assertTransferSubCmd =
    --   mkCommandParser "AssertTransfer"
    --   (AssertTransfer <$> parseSomeTransferParams)
    --   "Generate the parameter for the Whitelist contract: AssertTransfer"

    -- assertReceiverSubCmd =
    --   mkCommandParser "AssertReceiver"
    --   (AssertReceiver <$> parseAddress "receiver")
    --   "Generate the parameter for the Whitelist contract: AssertReceiver"

    assertReceiversSubCmd =
      mkCommandParser "AssertReceivers"
      (AssertReceivers <$>
        Opt.option
          (parseList' Opt.auto)
          (mconcat
            [ Opt.long "receivers"
            , Opt.metavar "[Address]"
            , Opt.help $ "Assert that all users are whitelisted and " <>
                "not blacklisted, or the issuer"
            ]
          )
      )
      "Generate the parameter for the Whitelist contract: AssertReceivers"

    setIssuerSubCmd =
      mkCommandParser "SetIssuer"
      (SetIssuer <$>
        parseSomeContractParam "newIssuer" <*>
        parseBool "wrapped"
      )
      "Generate the (wrapped) parameter for the Whitelist contract: SetIssuer"

    updateUserSubCmd =
      mkCommandParser "UpdateUser"
      (UpdateUser <$>
        parseSomeContractParam "newUser" <*>
        parseMaybe (parseNatural "newUserWhitelistId") <*>
        parseBool "wrapped"
      )
      "Generate the (wrapped) parameter for the Whitelist contract: UpdateUser"

    setWhitelistOutboundSubCmd =
      mkCommandParser "SetWhitelistOutbound"
      (SetWhitelistOutbound <$>
        (WhitelistOutboundParams <$>
          parseNatural "whitelistId" <*>
          (parseMaybe (Whitelist.mkOutboundWhitelists <$>
            parseBool "restricted" <*>
            Opt.option
              (parseList' Opt.auto)
              (mconcat
                [ Opt.long "outboundWhitelist"
                , Opt.metavar "[Natural]"
                , Opt.help "List of allowed outbound whitelists"
                ]
              )
          ))
        ) <*>
        parseBool "wrapped"
      )
      "Generate the (wrapped) parameter for the Whitelist contract: SetWhitelistOutbound"

    setAdminSubCmd =
      mkCommandParser "SetAdmin"
      (SetAdmin <$>
        parseAddress "admin" <*>
        parseBool "wrapped"
      )
      "Generate the (wrapped) parameter for the Whitelist contract: SetAdmin"

    getIssuerSubCmd =
      mkCommandParser "GetIssuer"
      (GetIssuer <$>
        parseView_ (Proxy @Address) <*>
        parseBool "wrapped"
      )
      "Generate the (wrapped) parameter for the Whitelist contract: GetIssuer"

    getUserSubCmd =
      mkCommandParser "GetUser"
      (GetUser <$>
        parseView @Address @(Maybe Whitelist.WhitelistId) (parseAddress "user") <*> -- (parseSomeContractParam "user")
        parseBool "wrapped"
      )
      "Generate the (wrapped) parameter for the Whitelist contract: GetUser"

    assertFilterlistSubCmd =
      mkCommandParser "AssertFilterlist"
      (AssertFilterlist <$>
        (Whitelist.AssertFilterlistParams <$>
          parseNatural "filterlistId" <*>
          parseMaybe (Whitelist.OutboundWhitelists <$>
            parseBool "unrestricted" <*>
            (Set.fromList <$> parseNaturals "allowedWhitelists")
        )) <*>
        parseBool "wrapped"
      )
      "Generate the (wrapped) parameter for the Whitelist contract: AssertFilterlist"

    getAdminSubCmd =
      mkCommandParser "GetAdmin"
      (GetAdmin <$>
        parseView_ (Proxy @Address) <*>
        parseBool "wrapped"
      )
      "Generate the (wrapped) parameter for the Whitelist contract: GetAdmin"

    wrappedParamSubCmd =
      mkCommandParser "WrappedParam"
      (WrappedParam <$> parseSomeContractParam "wrappedParam")
      ("Generate a wrapped parameter for the Whitelist contract, given the " <>
      "original contract's parameter")

infoMod :: Opt.InfoMod CmdLnArgs
infoMod = mconcat
  [ Opt.fullDesc
  , Opt.progDesc "Whitelist contract CLI interface"
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
    printLorentzContract forceOneLine (Whitelist.whitelistContract @Address) -- (Value t))
  Init {..} ->
    fromSomeStorage initialStorage $ \(initialStorage' :: Whitelist.Storage (Value s)) ->
      assertIsComparable @s $
      case initialWrappedStorage of
        Nothing ->
          TL.putStrLn . printLorentzValue @(Whitelist.Storage (Value s)) forceSingleLine $
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
  --   fromSomeTransferParams assertTransferParams $ \(assertTransferParams' :: Whitelist.TransferParams (Value t)) ->
  --   TL.putStrLn . printLorentzValue forceSingleLine $
  --   Whitelist.AssertTransfer assertTransferParams'
  AssertReceivers {..} ->
    TL.putStrLn . printLorentzValue forceSingleLine $
    Whitelist.AssertReceivers receivers
  SetIssuer {..} ->
    fromSomeContractParam newIssuer $ \(newIssuer' :: Value t) ->
      let st = sing @t in
      withDict (singIT st) $
      withDict (singTypeableT st) $
      if wrapped
         then
           TL.putStrLn . printLorentzValue @(Wrapper.Parameter () (Value t)) forceSingleLine $
           Wrapper.WhitelistParameter $
           Whitelist.SetIssuer newIssuer'
         else
           TL.putStrLn . printLorentzValue @(Whitelist.Parameter (Value t)) forceSingleLine $
           Whitelist.OtherParameter $
           Whitelist.SetIssuer newIssuer'
  UpdateUser {..} ->
    fromSomeContractParam newUser $ \(newUser' :: Value t) ->
      let st = sing @t in
      withDict (singIT st) $
      withDict (singTypeableT st) $
      if wrapped
         then
           TL.putStrLn . printLorentzValue @(Wrapper.Parameter () (Value t)) forceSingleLine $
           Wrapper.WhitelistParameter $
           Whitelist.UpdateUser $
           Whitelist.UpdateUserParams newUser' newUserWhitelistId
         else
           TL.putStrLn . printLorentzValue @(Whitelist.Parameter (Value t)) forceSingleLine $
           Whitelist.OtherParameter $
           Whitelist.UpdateUser $
           Whitelist.UpdateUserParams newUser' newUserWhitelistId
  SetWhitelistOutbound {..} ->
    if wrapped
       then
         TL.putStrLn . printLorentzValue @(Wrapper.Parameter () ()) forceSingleLine $
         Wrapper.WhitelistParameter $
         Whitelist.SetWhitelistOutbound whitelistOutboundParams
       else
         TL.putStrLn . printLorentzValue @(Whitelist.Parameter ()) forceSingleLine $
         Whitelist.OtherParameter $
         Whitelist.SetWhitelistOutbound whitelistOutboundParams
  SetAdmin {..} ->
    if wrapped
       then
         TL.putStrLn . printLorentzValue @(Wrapper.Parameter () ()) forceSingleLine $
         Wrapper.WhitelistParameter $
         Whitelist.SetAdmin admin
       else
         TL.putStrLn . printLorentzValue @(Whitelist.Parameter ()) forceSingleLine $
         Whitelist.OtherParameter $
         Whitelist.SetAdmin admin
  GetIssuer {..} ->
    if wrapped
       then
         TL.putStrLn . printLorentzValue @(Wrapper.Parameter () Address) forceSingleLine $
         Wrapper.WhitelistParameter $
         Whitelist.GetIssuer viewIssuer
       else
         TL.putStrLn . printLorentzValue @(Whitelist.Parameter Address) forceSingleLine $
         Whitelist.OtherParameter $
         Whitelist.GetIssuer viewIssuer
  GetUser {..} ->
    case viewUser of
      View viewParam addr' ->
        -- fromSomeContractParam viewParam $ \(viewParam' :: Value t) ->
          let viewUser' = View viewParam addr'
           in if wrapped
                 then
                   TL.putStrLn . printLorentzValue @(Wrapper.Parameter () Address) forceSingleLine $
                   Wrapper.WhitelistParameter $
                   Whitelist.GetUser viewUser'
                 else
                   TL.putStrLn . printLorentzValue @(Whitelist.Parameter Address) forceSingleLine $
                   Whitelist.OtherParameter $
                   Whitelist.GetUser viewUser'
  AssertFilterlist {..} ->
    if wrapped
       then
         TL.putStrLn . printLorentzValue @(Wrapper.Parameter () ()) forceSingleLine $
         Wrapper.WhitelistParameter $
         Whitelist.AssertFilterlist assertFilterlist
       else
         TL.putStrLn . printLorentzValue @(Whitelist.Parameter ()) forceSingleLine $
         Whitelist.OtherParameter $
         Whitelist.AssertFilterlist assertFilterlist
  GetAdmin {..} ->
    if wrapped
       then
         TL.putStrLn . printLorentzValue @(Wrapper.Parameter () ()) forceSingleLine $
         Wrapper.WhitelistParameter $
         Whitelist.GetAdmin viewAdmin
       else
         TL.putStrLn . printLorentzValue @(Whitelist.Parameter ()) forceSingleLine $
         Whitelist.OtherParameter $
         Whitelist.GetAdmin viewAdmin
  WrappedParam {..} ->
    fromSomeContractParam wrappedParam $ \(wrappedParam' :: Value t) ->
      let st = sing @t in
      withDict (singIT st) $
      withDict (singTypeableT st) $
      TL.putStrLn . printLorentzValue @(Wrapper.Parameter (Value t) ()) forceSingleLine $
      Wrapper.WrappedParameter wrappedParam'
  where
    forceSingleLine = True

