{-# OPTIONS -Wno-orphans -Wno-missing-export-lists #-}

module Lorentz.Contracts.Transferlist.Parsers where

import Prelude (FilePath, ($), Natural, error, runReaderT)
import Data.Char
import Data.Bool
import Data.Either
import Data.String
import Data.Maybe
import Data.Monoid
import Data.Functor
import Control.Applicative
import Control.Monad
import Text.Read
import Text.Show
import Data.Function
import Data.Proxy

import Lorentz (Address, View(..), GetDefaultEntryPointArg, NiceParameterFull, TAddress(..), Value, callingDefTAddress, def)
import Michelson.Typed.T
import Michelson.Typed.Annotation
import Michelson.Typed.Sing
import Michelson.Typed.EntryPoints (EpAddress(..))
import qualified Michelson.Typed.EntryPoints as EntryPoints
import Michelson.Parser
import Michelson.TypeCheck
import Michelson.Macro

import Text.Megaparsec (eof)
import qualified Options.Applicative as Opt
import qualified Data.Text as T
import Data.Constraint
import Data.Singletons

import Lorentz.Contracts.SomeContractStorage
import Lorentz.Contracts.SomeContractParam
import Michelson.Typed.Scope.Missing
import Tezos.Crypto.Orphans

-- | Parse and typecheck a Michelson value
parseTypeCheckValue ::
     forall t. (SingI t)
  => Parser (Value t)
parseTypeCheckValue =
  (>>= either (fail . show) return) $
  runTypeCheckIsolated . flip runReaderT def . typeCheckValue . expandValue <$>
  (value <* eof)

instance Read Address where
  readPrec = readP_to_Prec $ const readAddressP

-- | Parse an `Address` argument, given its field name
parseAddress :: String -> Opt.Parser Address
parseAddress name =
  Opt.option Opt.auto $
  mconcat
    [ Opt.long name
    , Opt.metavar "ADDRESS"
    , Opt.help $ "Address of the " <> name <> "."
    ]

-- | Parse an address which can be suffixed with entrypoint name
-- (e.g. "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU%entrypoint").
--
-- See `EntryPoints.parseEpAddress`
instance Read EpAddress where
  readPrec = do
    str <- readPrec
    case EntryPoints.parseEpAddress $ fromString str of
      Left err -> fail $ show err
      Right result' -> return result'

-- | See `EntryPoints.parseEpAddress`
parseEpAddress :: String -> Opt.Parser EpAddress
parseEpAddress name =
  Opt.option Opt.auto $
  mconcat
    [ Opt.long name
    , Opt.metavar "EPADDRESS"
    , Opt.help $ "Address (which can be suffixed with an entrypoint name) of the " <> name <> "."
    ]

-- | Parse a `View` by parsing its arguments and @"callback-contract"@ address
parseView :: forall a r. NiceParameterFull r => Opt.Parser a -> Opt.Parser (View a (GetDefaultEntryPointArg r))
parseView parseArg =
  View <$> parseArg <*> fmap (callingDefTAddress . TAddress @r) (parseAddress "callback-contract")

-- | Parse a `View_`
parseView_ :: forall r. NiceParameterFull r => Proxy r -> Opt.Parser (View () (GetDefaultEntryPointArg r))
parseView_ _ = parseView @() @r $ pure ()

-- | Parse a `Bool` (optional) argument, given its field name
parseBool :: String -> Opt.Parser Bool
parseBool name =
  Opt.option Opt.auto $
  mconcat
    [ Opt.long name
    , Opt.metavar "BOOL"
    , Opt.help $
      "Bool representing whether the contract is initially " <> name <> "."
    ]

-- | Parse a `String`
parseString :: String -> Opt.Parser String
parseString name = Opt.strOption $ mconcat
  [ Opt.long name
  , Opt.metavar "STRING"
  , Opt.help $ "String representing the contract's initial " <> name <> "."
  ]

-- | Parse a list of `String`'s
parseStrings :: String -> Opt.Parser [String]
parseStrings name = Opt.option Opt.auto $ mconcat
  [ Opt.long name
  , Opt.metavar "[STRING]"
  , Opt.help $ "List of String's representing " <> name <> "."
  ]

-- | Parse a natural number argument, given its field name
parseNatural :: String -> Opt.Parser Natural
parseNatural name =
  Opt.option Opt.auto $
  mconcat
    [ Opt.long name
    , Opt.metavar "NATURAL"
    , Opt.help $ "Natural number representing " <> name <> "."
    ]

-- | Parse a list of natural numbers, given its field name
parseNaturals :: String -> Opt.Parser [Natural]
parseNaturals name =
  Opt.option Opt.auto $
  mconcat
    [ Opt.long name
    , Opt.metavar "[NATURAL]"
    , Opt.help $ "List of Natural numbers representing " <> name <> "."
    ]

-- | Parse the output `FilePath`
outputOptions :: Opt.Parser (Maybe FilePath)
outputOptions = optional . Opt.strOption $ mconcat
  [ Opt.short 'o'
  , Opt.long "output"
  , Opt.metavar "FILEPATH"
  , Opt.help "File to use as output. If not specified, stdout is used."
  ]

-- | Parse whether to output on one line
onelineOption :: Opt.Parser Bool
onelineOption = Opt.switch (
  Opt.long "oneline" <>
  Opt.help "Force single line output")

-- | Parse a natural number argument, given its field name
parseAuto :: Read a => String -> String -> Opt.Parser a
parseAuto name description =
  Opt.option Opt.auto $
  mconcat
    [ Opt.long name
    , Opt.metavar $ toUpper <$> name
    , Opt.help description
    ]

-- | Parse `SomeContractStorage`, see `parseSomeContractParam`
parseSomeContractStorage :: String -> Opt.Parser SomeContractStorage
parseSomeContractStorage name =
  (\(SomeSing (st :: Sing t)) paramStr ->
    withDict (singIT st) $
    withDict (singTypeableT st) $
    assertOpAbsense @t $
    assertBigMapAbsense @t $
    let parsedParam = parseNoEnv
          (parseTypeCheckValue @t)
          name
          paramStr
     in let param = either (error . T.pack . show) id parsedParam
     in SomeContractStorage param -- param (st, NStar) (Dict, Dict)
  ) <$>
  parseSomeT name <*>
  Opt.strOption @T.Text
    (mconcat
      [ Opt.long name
      , Opt.metavar "Michelson Value"
      , Opt.help $ "The Michelson Value: " <> name
      ])

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
  Opt.strOption @T.Text
    (mconcat
      [ Opt.long $ name <> "Type"
      , Opt.metavar "Michelson Type"
      , Opt.help $ "The Michelson Type of " <> name
      ])

parseSomeContractParam :: String -> Opt.Parser SomeContractParam
parseSomeContractParam name =
  (\(SomeSing (st :: Sing t)) paramStr ->
    withDict (singIT st) $
    withDict (singTypeableT st) $
    assertOpAbsense @t $
    assertBigMapAbsense @t $
    let parsedParam = parseNoEnv
          (parseTypeCheckValue @t)
          name
          paramStr
     in let param = either (error . T.pack . show) id parsedParam
     in SomeContractParam param (st, starNotes) (Dict, Dict)
  ) <$>
  parseSomeT name <*>
  Opt.strOption @T.Text
    (mconcat
      [ Opt.long name
      , Opt.metavar "Michelson Value"
      , Opt.help $ "The Michelson Value: " <> name
      ])

