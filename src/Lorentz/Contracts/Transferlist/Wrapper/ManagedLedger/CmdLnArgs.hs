{-# OPTIONS -Wno-partial-fields -Wno-orphans #-}

module Lorentz.Contracts.Transferlist.Wrapper.ManagedLedger.CmdLnArgs where

import Lorentz hiding (get)
import Util.IO

import qualified Options.Applicative as Opt
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Map.Strict as Map

import qualified Lorentz.Contracts.ManagedLedger as ManagedLedger

import Lorentz.Contracts.Transferlist.Parsers
import qualified Lorentz.Contracts.Transferlist.Wrapper.ManagedLedger as Transferlisted
import qualified Lorentz.Contracts.Transferlist.Types as Transferlist
import qualified Lorentz.Contracts.Transferlist.CmdLnArgs as Transferlist
import qualified Lorentz.Contracts.Transferlist.Wrapper as Wrapper

instance HasTypeAnn ManagedLedger.Parameter

data CmdLnArgs
  = Print (Maybe FilePath) Bool
  | Init
      { initialStorage :: !(Transferlist.Storage Address)
      , managedLedgerAdmin :: !Address
      , balances :: !(Map Address Natural)
      }

argParser :: Opt.Parser CmdLnArgs
argParser = Opt.hsubparser $ mconcat
  [ printSubCmd
  , initSubCmd
  ]
  where
    mkCommandParser commandName parser desc =
      Opt.command commandName $
      Opt.info (Opt.helper <*> parser) $
      Opt.progDesc desc

    printSubCmd =
      mkCommandParser "print"
      (Print <$> outputOptions <*> onelineOption)
      "Dump the Oracle contract in form of Michelson code"

    initSubCmd =
      mkCommandParser "init"
      (Init <$>
        Transferlist.parseStorage parseAddress <*>
        parseAddress "managedLedgerAdmin" <*>
        Opt.option
          (Map.fromList <$> Opt.auto)
          (mconcat
            [ Opt.long "balances"
            , Opt.metavar "[(ADDRESS, NATURAL)]"
            , Opt.help "The initial balances"
            ])
      )
      "Initial storage for the Transferlist Wrapped ManagedLedger contract"

infoMod :: Opt.InfoMod CmdLnArgs
infoMod = mconcat
  [ Opt.fullDesc
  , Opt.progDesc "Transferlist Wrapped ManagedLedger contract CLI interface"
  ]

runCmdLnArgs :: CmdLnArgs -> IO ()
runCmdLnArgs = \case
  Print mOutput forceOneLine ->
    maybe TL.putStrLn writeFileUtf8 mOutput $
    printLorentzContract forceOneLine Transferlisted.transferlistedManagedLedgerContract
  Init {..} ->
    TL.putStrLn $
    printLorentzValue @(Wrapper.Storage ManagedLedger.Storage Address) forceSingleLine $
    Wrapper.Storage
      (ManagedLedger.mkStorage managedLedgerAdmin balances)
      initialStorage
  where
    forceSingleLine = True

