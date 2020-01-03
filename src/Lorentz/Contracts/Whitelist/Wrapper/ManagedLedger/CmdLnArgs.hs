{-# OPTIONS -Wno-missing-export-lists -Wno-unused-do-bind -Wno-partial-fields -Wno-orphans #-}

module Lorentz.Contracts.Whitelist.Wrapper.ManagedLedger.CmdLnArgs where

import Lorentz hiding (get)
import Util.IO

import qualified Options.Applicative as Opt
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Map.Strict as Map

import Lorentz.Contracts.Util ()
import Lorentz.Contracts.Parse
import qualified Lorentz.Contracts.ManagedLedger as ManagedLedger

import qualified Lorentz.Contracts.Whitelist.Wrapper.ManagedLedger as Whitelisted
import qualified Lorentz.Contracts.Whitelist as Whitelist
import qualified Lorentz.Contracts.Whitelist.CmdLnArgs as Whitelist
import qualified Lorentz.Contracts.Whitelist.Wrapper as Wrapper

data CmdLnArgs
  = Print (Maybe FilePath) Bool
  | Init
      { initialStorage :: !(Whitelist.Storage Address)
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
        Whitelist.parseStorage parseAddress <*>
        parseAddress "managedLedgerAdmin" <*>
        Opt.option
          (Map.fromList <$> Opt.auto)
          (mconcat
            [ Opt.long "balances"
            , Opt.metavar "[(ADDRESS, NATURAL)]"
            , Opt.help "The initial balances"
            ])
      )
      "Initial storage for the Whitelist Wrapped ManagedLedger contract"

infoMod :: Opt.InfoMod CmdLnArgs
infoMod = mconcat
  [ Opt.fullDesc
  , Opt.progDesc "Whitelist Wrapped ManagedLedger contract CLI interface"
  ]

runCmdLnArgs :: CmdLnArgs -> IO ()
runCmdLnArgs = \case
  Print mOutput forceOneLine ->
    maybe TL.putStrLn writeFileUtf8 mOutput $
    printLorentzContract forceOneLine Whitelisted.whitelistedManagedLedgerContract
  Init {..} ->
    TL.putStrLn $
    printLorentzValue @(Wrapper.Storage ManagedLedger.Storage Address) forceSingleLine $
    Wrapper.Storage
      (ManagedLedger.mkStorage managedLedgerAdmin balances)
      initialStorage
  where
    forceSingleLine = True

