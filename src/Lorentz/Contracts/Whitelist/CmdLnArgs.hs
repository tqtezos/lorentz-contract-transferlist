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
import Michelson.Typed.Annotation
import Michelson.Typed.Arith
import Michelson.Typed.Haskell.Value
import Michelson.Typed.Scope
import Michelson.Typed.Sing
import Michelson.Typed.T
import Michelson.Typed.Value
import Util.IO
import qualified Michelson.Untyped.Type as U

import qualified Options.Applicative as Opt
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import Data.Constraint
import Data.Singletons

import Lorentz.Contracts.Util ()
import Lorentz.Contracts.SomeContractParam
import Lorentz.Contracts.Parse
import qualified Lorentz.Contracts.GenericMultisig.Wrapper as G

import qualified Lorentz.Contracts.Whitelist as Whitelist
import qualified Lorentz.Contracts.Whitelist.Types as Whitelist
import Lorentz.Contracts.Whitelist.Types
  ( OutboundWhitelists(..)
  , OutboundWhitelists(..)
  , View_
  , WhitelistId
  , WhitelistOutboundParams(..)
  )
import qualified Lorentz.Contracts.Whitelist.Wrapper as Wrapper

-- | TODO: merge upstream into morley
instance IsoCValue (Value ('Tc ct)) where
  type ToCT (Value ('Tc ct)) = ct
  toCVal (VC xs) = xs
  fromCVal = VC

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

instance (Typeable ct, CompareOp ct) => CompareOpHs (Value ('Tc ct)) where

-- | `SingI` implies `CompareOp` forall `CT`
compareOpCT :: forall ct. SingI ct :- CompareOp ct
compareOpCT = Sub $
  case sing @ct of
    SCInt -> Dict
    SCNat -> Dict
    SCString -> Dict
    SCBytes -> Dict
    SCMutez -> Dict
    SCBool -> Dict
    SCKeyHash -> Dict
    SCTimestamp -> Dict
    SCAddress -> Dict

-- | Assert `HasNoOp`
assertOpAbsense :: forall (t :: T) a. SingI t => (HasNoOp t => a) -> a
assertOpAbsense f =
  case opAbsense (sing @t) of
    Nothing -> error "assertOpAbsense"
    Just Dict -> forbiddenOp @t f

-- | Assert `HasNoBigMap`
assertBigMapAbsense :: forall (t :: T) a. SingI t => (HasNoBigMap t => a) -> a
assertBigMapAbsense f =
  case bigMapAbsense (sing @t) of
    Nothing -> error "assertBigMapAbsense"
    Just Dict -> forbiddenBigMap @t f

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
    _ -> error "assertIsComparable"

-- | `Sing` implies `Typeable`
singTypeableCT :: forall (t :: CT). Sing t -> Dict (Typeable t)
singTypeableCT SCInt = Dict
singTypeableCT SCNat = Dict
singTypeableCT SCString = Dict
singTypeableCT SCBytes = Dict
singTypeableCT SCMutez = Dict
singTypeableCT SCBool = Dict
singTypeableCT SCKeyHash = Dict
singTypeableCT SCTimestamp = Dict
singTypeableCT SCAddress = Dict

-- | `Sing` implies `Typeable`
singTypeableT :: forall (t :: T). Sing t -> Dict (Typeable t)
singTypeableT (STc ct) =
  withDict (singTypeableCT ct) $
  Dict
singTypeableT STKey = Dict
singTypeableT STUnit = Dict
singTypeableT STSignature = Dict
singTypeableT STChainId = Dict
singTypeableT (STOption st) =
  withDict (singTypeableT st) $
  Dict
singTypeableT (STList st) =
  withDict (singTypeableT st) $
  Dict
singTypeableT (STSet st) =
  withDict (singTypeableCT st) $
  Dict
singTypeableT STOperation  = Dict
singTypeableT (STContract st) =
  withDict (singTypeableT st) $
  Dict
singTypeableT (STPair st su) =
  withDict (singTypeableT st) $
  withDict (singTypeableT su) $
  Dict
singTypeableT (STOr st su) =
  withDict (singTypeableT st) $
  withDict (singTypeableT su) $
  Dict
singTypeableT (STLambda st su) =
  withDict (singTypeableT st) $
  withDict (singTypeableT su) $
  Dict
singTypeableT (STMap st su) =
  withDict (singTypeableCT st) $
  withDict (singTypeableT su) $
  Dict
singTypeableT (STBigMap st su) =
  withDict (singTypeableCT st) $
  withDict (singTypeableT su) $
  Dict

-- | `Sing` implies `SingI`
singICT :: forall (t :: CT). Sing t -> Dict (SingI t)
singICT SCInt = Dict
singICT SCNat = Dict
singICT SCString = Dict
singICT SCBytes = Dict
singICT SCMutez = Dict
singICT SCBool = Dict
singICT SCKeyHash = Dict
singICT SCTimestamp = Dict
singICT SCAddress = Dict

-- | `Sing` implies `SingI`
singIT :: forall (t :: T). Sing t -> Dict (SingI t)
singIT (STc ct) =
  withDict (singICT ct) $
  Dict
singIT STKey = Dict
singIT STUnit = Dict
singIT STSignature = Dict
singIT STChainId = Dict
singIT (STOption st) =
  withDict (singIT st) $
  Dict
singIT (STList st) =
  withDict (singIT st) $
  Dict
singIT (STSet st) =
  withDict (singICT st) $
  Dict
singIT STOperation  = Dict
singIT (STContract st) =
  withDict (singIT st) $
  Dict
singIT (STPair st su) =
  withDict (singIT st) $
  withDict (singIT su) $
  Dict
singIT (STOr st su) =
  withDict (singIT st) $
  withDict (singIT su) $
  Dict
singIT (STLambda st su) =
  withDict (singIT st) $
  withDict (singIT su) $
  Dict
singIT (STMap st su) =
  withDict (singICT st) $
  withDict (singIT su) $
  Dict
singIT (STBigMap st su) =
  withDict (singICT st) $
  withDict (singIT su) $
  Dict

-- | Some `Whitelist.Storage` with a `KnownValue` constraint
data SomeStorage where
  SomeStorage :: (KnownValue (Value a))
    => Whitelist.Storage (Value a)
    -> SomeStorage

-- | Run `SomeStorage`
fromSomeStorage :: forall b. SomeStorage -> (forall a. (KnownValue (Value a)) => Whitelist.Storage (Value a) -> b) -> b
fromSomeStorage (SomeStorage xs) f = f xs

-- | Some contract storage with `SingI` and `HasNoOp` constraints
data SomeContractStorage where
  SomeContractStorage :: (SingI a, HasNoOp a)
    => Value a
    -> SomeContractStorage

-- | Run `SomeContractStorage`
fromSomeContractStorage :: forall b. SomeContractStorage -> (forall a. (SingI a, HasNoOp a) => Value a -> b) -> b
fromSomeContractStorage (SomeContractStorage xs) f = f xs

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
  | AssertTransfer
      { assertTransferParams :: !SomeTransferParams
      }
  | AssertReceiver
      { receiver :: !Address
      }
  | AssertReceivers
      { receivers :: ![Address]
      }
  | SetIssuer
      { newIssuer :: !SomeContractParam
      , wrapped :: !Bool
      }
  | AddUser
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
      { viewIssuer :: !(View_ ())
      , wrapped :: !Bool
      }
  | GetUser
      { viewUser :: !(View SomeContractParam (Maybe WhitelistId))
      , wrapped :: !Bool
      }
  | GetWhitelist
      { viewWhitelist :: !(View WhitelistId (Maybe OutboundWhitelists))
      , wrapped :: !Bool
      }
  | GetAdmin
      { viewAdmin :: !(View_ Address)
      , wrapped :: !Bool
      }
  | WrappedParam
      { wrappedParam :: !SomeContractParam
      }

-- | Make a type non-explicit
unExplicitType :: U.Type -> U.T
unExplicitType =
  \case
    U.Type t _ -> t

-- | Convert a `U.Comparable` to `CT`
fromUntypedComparable :: U.Comparable -> CT
fromUntypedComparable (U.Comparable ct _) = ct

-- | Convert a `U.Type` to `T`
fromUntypedT' :: U.Type -> T
fromUntypedT' = fromUntypedT . unExplicitType

-- | Convert a `U.T` to `T`
fromUntypedT :: U.T -> T
fromUntypedT (U.Tc ct) = Tc ct
fromUntypedT U.TKey = TKey
fromUntypedT U.TUnit = TUnit
fromUntypedT U.TChainId = TChainId
fromUntypedT U.TSignature = TSignature
fromUntypedT (U.TOption x) = TOption $ fromUntypedT' x
fromUntypedT (U.TList x) = TList $ fromUntypedT' x
fromUntypedT (U.TSet ct) = TSet $ fromUntypedComparable ct
fromUntypedT U.TOperation = TOperation
fromUntypedT (U.TContract x) = TContract $ fromUntypedT' x
fromUntypedT (U.TPair _ _ x y) = TPair (fromUntypedT' x) (fromUntypedT' y)
fromUntypedT (U.TOr _ _ x y) = TOr (fromUntypedT' x) (fromUntypedT' y)
fromUntypedT (U.TLambda x y) = TLambda (fromUntypedT' x) (fromUntypedT' y)
fromUntypedT (U.TMap ct x) = TMap (fromUntypedComparable ct) $ fromUntypedT' x
fromUntypedT (U.TBigMap ct x) = TBigMap (fromUntypedComparable ct) $ fromUntypedT' x

parseList' :: Read a => Opt.ReadM a -> Opt.ReadM [a]
parseList' _ = Opt.auto

-- | Parse some `T`
parseSomeT :: String -> Opt.Parser (SomeSing T)
parseSomeT name =
  (\typeStr ->
    let parsedType = parseNoEnv
          type_
          name
          typeStr
     in let type' = either (error . T.pack . show) unExplicitType parsedType
     in withSomeSingT (fromUntypedT type') SomeSing
  ) <$>
  Opt.strOption @Text
    (mconcat
      [ Opt.long $ name ++ "Type"
      , Opt.metavar "Michelson Type"
      , Opt.help $ "The Michelson Type of " ++ name
      ])

-- | Parse `SomeContractParam`, given an argument for the type
parseSomeContractParam :: String -> Opt.Parser SomeContractParam
parseSomeContractParam name =
  (\(SomeSing (st :: Sing t)) paramStr ->
    withDict (singIT st) $
    withDict (singTypeableT st) $
    assertOpAbsense @t $
    assertBigMapAbsense @t $
    let parsedParam = parseNoEnv
          (G.parseTypeCheckValue @t)
          name
          paramStr
     in let param = either (error . T.pack . show) id parsedParam
     in SomeContractParam param (st, starNotes) (Dict, Dict)
  ) <$>
  parseSomeT name <*>
  Opt.strOption @Text
    (mconcat
      [ Opt.long name
      , Opt.metavar "Michelson Value"
      , Opt.help $ "The Michelson Value: " ++ name
      ])

-- | Parse and return `Just` or `Nothing` if it fails
parseMaybe :: Alternative f => f a -> f (Maybe a)
parseMaybe p = fmap Just p <|> pure Nothing

-- | Parse `SomeContractStorage`, see `parseSomeContractParam`
parseSomeContractStorage :: String -> Opt.Parser SomeContractStorage
parseSomeContractStorage name =
  (\(SomeSing (st :: Sing t)) paramStr ->
    withDict (singIT st) $
    withDict (singTypeableT st) $
    assertOpAbsense @t $
    assertBigMapAbsense @t $
    let parsedParam = parseNoEnv
          (G.parseTypeCheckValue @t)
          name
          paramStr
     in let param = either (error . T.pack . show) id parsedParam
     in SomeContractStorage param -- param (st, NStar) (Dict, Dict)
  ) <$>
  parseSomeT name <*>
  Opt.strOption @Text
    (mconcat
      [ Opt.long name
      , Opt.metavar "Michelson Value"
      , Opt.help $ "The Michelson Value: " ++ name
      ])

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
            (G.parseTypeCheckValue @t)
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
                                     (G.parseTypeCheckValue @t)
                                     name
                                     $ T.pack fromStr
                                 , parseNoEnv
                                     (G.parseTypeCheckValue @t)
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

-- | Parse a `View_`
parseView_ :: NiceParameter r => Opt.Parser (View () r)
parseView_ = parseView $ pure ()

argParser :: Opt.Parser CmdLnArgs
argParser = Opt.hsubparser $ mconcat
  [ printSubCmd
  , initSubCmd
  , assertTransferSubCmd
  , assertReceiverSubCmd
  , assertReceiversSubCmd
  , setIssuerSubCmd
  , addUserSubCmd
  , setWhitelistOutboundSubCmd
  , setAdminSubCmd
  , getIssuerSubCmd
  , getUserSubCmd
  , getWhitelistSubCmd
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

    assertTransferSubCmd =
      mkCommandParser "AssertTransfer"
      (AssertTransfer <$> parseSomeTransferParams)
      "Generate the parameter for the Whitelist contract: AssertTransfer"

    assertReceiverSubCmd =
      mkCommandParser "AssertReceiver"
      (AssertReceiver <$> parseAddress "receiver")
      "Generate the parameter for the Whitelist contract: AssertReceiver"

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

    addUserSubCmd =
      mkCommandParser "AddUser"
      (AddUser <$>
        parseSomeContractParam "newUser" <*>
        parseMaybe (parseNatural "newUserWhitelistId") <*>
        parseBool "wrapped"
      )
      "Generate the (wrapped) parameter for the Whitelist contract: AddUser"

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
        parseView_ <*>
        parseBool "wrapped"
      )
      "Generate the (wrapped) parameter for the Whitelist contract: GetIssuer"

    getUserSubCmd =
      mkCommandParser "GetUser"
      (GetUser <$>
        parseView (parseSomeContractParam "user") <*>
        parseBool "wrapped"
      )
      "Generate the (wrapped) parameter for the Whitelist contract: GetUser"

    getWhitelistSubCmd =
      mkCommandParser "GetWhitelist"
      (GetWhitelist <$>
        parseView (parseNatural "whitelistId") <*>
        parseBool "wrapped"
      )
      "Generate the (wrapped) parameter for the Whitelist contract: GetWhitelist"

    getAdminSubCmd =
      mkCommandParser "GetAdmin"
      (GetAdmin <$>
        parseView_ <*>
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
    assertOpAbsense @t $
    assertBigMapAbsense @t $
    assertIsComparable @t $
    withDict (compareOpCT @(ToCT (Value t))) $
    maybe TL.putStrLn writeFileUtf8 mOutput $
    printLorentzContract forceOneLine (Whitelist.whitelistContract @(Value t))
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
  AssertTransfer {..} ->
    fromSomeTransferParams assertTransferParams $ \(assertTransferParams' :: Whitelist.TransferParams (Value t)) ->
    TL.putStrLn . printLorentzValue forceSingleLine $
    Whitelist.AssertTransfer assertTransferParams'
  AssertReceiver {..} ->
    TL.putStrLn . printLorentzValue forceSingleLine $
    Whitelist.AssertReceiver receiver
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
  AddUser {..} ->
    fromSomeContractParam newUser $ \(newUser' :: Value t) ->
      let st = sing @t in
      withDict (singIT st) $
      withDict (singTypeableT st) $
      if wrapped
         then
           TL.putStrLn . printLorentzValue @(Wrapper.Parameter () (Value t)) forceSingleLine $
           Wrapper.WhitelistParameter $
           Whitelist.AddUser $
           Whitelist.UpdateUserParams newUser' newUserWhitelistId
         else
           TL.putStrLn . printLorentzValue @(Whitelist.Parameter (Value t)) forceSingleLine $
           Whitelist.OtherParameter $
           Whitelist.AddUser $
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
         TL.putStrLn . printLorentzValue @(Wrapper.Parameter () ()) forceSingleLine $
         Wrapper.WhitelistParameter $
         Whitelist.GetIssuer viewIssuer
       else
         TL.putStrLn . printLorentzValue @(Whitelist.Parameter ()) forceSingleLine $
         Whitelist.OtherParameter $
         Whitelist.GetIssuer viewIssuer
  GetUser {..} ->
    case viewUser of
      View viewParam addr' ->
        fromSomeContractParam viewParam $ \(viewParam' :: Value t) ->
          let viewUser' = View viewParam' addr'
           in if wrapped
                 then
                   TL.putStrLn . printLorentzValue @(Wrapper.Parameter () (Value t)) forceSingleLine $
                   Wrapper.WhitelistParameter $
                   Whitelist.GetUser viewUser'
                 else
                   TL.putStrLn . printLorentzValue @(Whitelist.Parameter (Value t)) forceSingleLine $
                   Whitelist.OtherParameter $
                   Whitelist.GetUser viewUser'
  GetWhitelist {..} ->
    if wrapped
       then
         TL.putStrLn . printLorentzValue @(Wrapper.Parameter () ()) forceSingleLine $
         Wrapper.WhitelistParameter $
         Whitelist.GetWhitelist viewWhitelist
       else
         TL.putStrLn . printLorentzValue @(Whitelist.Parameter ()) forceSingleLine $
         Whitelist.OtherParameter $
         Whitelist.GetWhitelist viewWhitelist
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

