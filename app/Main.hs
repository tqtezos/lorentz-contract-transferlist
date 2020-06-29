{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS -Wno-partial-fields -Wno-orphans -Wno-missing-export-lists #-}

module Main where

import Control.Applicative
import Control.Monad hiding (fail)
import Data.Function
import System.IO
import Prelude (die, displayException, catchAny)

import Lorentz
import Michelson.Typed.Scope
import Util.IO
import Michelson.Printer

import qualified Options.Applicative as Opt
import qualified Data.Text.Lazy as TL
import Text.PrettyPrint.ANSI.Leijen.Internal (Doc, linebreak)

import qualified Lorentz.Contracts.Filterlist.CmdLnArgs as FilterlistCmdLnArgs
import qualified Lorentz.Contracts.Filterlist.Wrapper.ManagedLedger.CmdLnArgs as FilterlistManagedLedgerCmdLnArgs

-- | Convert to a `Value`, untype, and render
showValue :: (IsoValue t, SingI (ToT t), HasNoOp (ToT t)) => t -> TL.Text
showValue = printTypedValue False . toVal

data CmdLnArgs
  = FilterlistCmdLnArgs { filterlistCmdLnArgs :: FilterlistCmdLnArgs.CmdLnArgs }
  | FilterlistManagedLedgerCmdLnArgs { filterlistManagedLedgerCmdLnArgs :: FilterlistManagedLedgerCmdLnArgs.CmdLnArgs }

argParser :: Opt.Parser CmdLnArgs
argParser = Opt.hsubparser $ mconcat
  [ Opt.command "Filterlist" $ fmap FilterlistCmdLnArgs $ Opt.info FilterlistCmdLnArgs.argParser FilterlistCmdLnArgs.infoMod
  , Opt.command "FilterlistManagedLedger" $ fmap FilterlistManagedLedgerCmdLnArgs $ Opt.info FilterlistManagedLedgerCmdLnArgs.argParser FilterlistManagedLedgerCmdLnArgs.infoMod
  ]
  where

programInfo :: Opt.ParserInfo CmdLnArgs
programInfo =
  -- Opt.info (Opt.helper <*> versionOption <*> argParser) $
  Opt.info (Opt.helper <*> argParser) $
  mconcat
    [ Opt.fullDesc
    , Opt.progDesc "Sale contract parameter generation helper"
    , Opt.header "Lorentz tools"
    , Opt.footerDoc usageDoc
    ]
  -- where
  --   versionOption =
  --     Opt.infoOption
  --       ("lorentz-contract-oracle-" <> versionStr)
  --       (Opt.long "version" <> Opt.help "Show version.")
  --   versionStr = "0.1.0.0" -- showVersion version

usageDoc :: Maybe Doc
usageDoc =
  Just $
  mconcat
    [ "You can use help for specific COMMAND"
    , linebreak
    , "EXAMPLE:"
    , linebreak
    , "  lorentz-contract-sale COMMAND --help"
    , linebreak
    ]

main :: IO ()
main = do
  hSetTranslit stdout
  hSetTranslit stderr

  cmdLnArgs <- Opt.execParser programInfo
  run cmdLnArgs `catchAny` (die . displayException)
  where
    run :: CmdLnArgs -> IO ()
    run =
      \case
        FilterlistCmdLnArgs {..} ->
          FilterlistCmdLnArgs.runCmdLnArgs filterlistCmdLnArgs
        FilterlistManagedLedgerCmdLnArgs {..} ->
          FilterlistManagedLedgerCmdLnArgs.runCmdLnArgs filterlistManagedLedgerCmdLnArgs

