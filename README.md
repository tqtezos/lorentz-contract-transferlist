
See the FA1.2 [Quick Start Tutorial](https://assets.tqtezos.com/token-contracts/1-fa12-lorentz) for more detail.

# The CLI

The CLI has two parts:
- `Whitelist`: Polymorphic whitelist contract and parameters
- `WhitelistManagedLedger`: The Whitelisted FA1.2 contract

```bash
❯❯❯ ./stack exec -- lorentz-contract-whitelist --help

Lorentz tools

Usage: lorentz-contract-whitelist COMMAND
  Sale contract parameter generation helper

Available options:
  -h,--help                Show this help text

Available commands:
  Whitelist                Whitelist contract CLI interface
  WhitelistManagedLedger   Whitelist Wrapped ManagedLedger contract CLI
                           interface

You can use help for specific COMMAND
EXAMPLE:
  lorentz-contract-sale COMMAND --help
```

The `WhitelistManagedLedger` part is only used to print and initialize a wrapped
`ManagedLedger`, i.e. FA1.2, contract.
The rest of the interface is provided in the `Whitelist` part:

```bash
❯❯❯ ./stack exec -- lorentz-contract-whitelist Whitelist --help
Usage: lorentz-contract-whitelist Whitelist COMMAND
  Whitelist contract CLI interface

Available options:
  -h,--help                Show this help text

Available commands:
  print                    Dump the Whitelist contract in form of Michelson code
  init                     Initial storage for the (wrapped) Whitelist contract:
                           pass 'initialWrappedStorage' for the wrapped version
  assertTransfer           Generate the parameter for the Whitelist contract:
                           AssertTransfer
  SetIssuer                Generate the (wrapped) parameter for the Whitelist
                           contract: SetIssuer
  AddUser                  Generate the (wrapped) parameter for the Whitelist
                           contract: AddUser
  SetWhitelistOutbound     Generate the (wrapped) parameter for the Whitelist
                           contract: SetWhitelistOutbound
  SetAdmin                 Generate the (wrapped) parameter for the Whitelist
                           contract: SetAdmin
  GetIssuer                Generate the (wrapped) parameter for the Whitelist
                           contract: GetIssuer
  GetUser                  Generate the (wrapped) parameter for the Whitelist
                           contract: GetUser
  GetWhitelist             Generate the (wrapped) parameter for the Whitelist
                           contract: GetWhitelist
  GetAdmin                 Generate the (wrapped) parameter for the Whitelist
                           contract: GetAdmin
  WrappedParam             Generate a wrapped parameter for the Whitelist
                           contract, given the original contract's parameter
```

# Standalone Whitelist Contract

An example initial storage:

```bash
❯❯❯ stack exec -- lorentz-contract-whitelist Whitelist init --issuer "\"$ALICE_ADDRESS\"" \
  --whitelists "[]" \
  --users "[]" \
  --admin "\"$ALICE_ADDRESS\"" \
  --initialStorageType 'address'

Pair (Pair "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr" { }) (Pair { } "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr")
```

To originate the contract:

```bash
$ tezos-client --wait none originate contract Whitelist \
  transferring 0 from $ALICE_ADDRESS running \
  "$(cat contracts/address_whitelist.tz)" \
  --init "$(stack exec -- lorentz-contract-whitelist Whitelist init --issuer "\"$ALICE_ADDRESS\"" \
  --whitelists "[]" \
  --users "[]" \
  --admin "\"$ALICE_ADDRESS\"" \
  --initialStorageType 'address')" --burn-cap 2.868

Waiting for the node to be bootstrapped before injection...
Current head: BKj8GASFzkn3 (timestamp: 2020-04-13T19:57:21-00:00, validation: 2020-04-13T19:57:37-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 77578 units (will add 100 for safety)
Estimated storage: 2868 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'ooxGu3cE8irRMcvwSwDUNqMKQkyQBuom6Swh1jikznrBisLqVzA'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for ooxGu3cE8irRMcvwSwDUNqMKQkyQBuom6Swh1jikznrBisLqVzA to be included --confirmations 30 --branch BKj8GASFzkn37cVU92mLmMhjLTQwfQpPjQnGTBwNpeSqWiuVYAm
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.010575
    Expected counter: 623948
    Gas limit: 77678
    Storage limit: 2888 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.010575
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,166) ... +ꜩ0.010575
    Origination:
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      Credit: ꜩ0
      Script:
        { ... }
        Initial storage:
          (Pair (Pair "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr" {})
                (Pair {} "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr"))
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1EYLosB86x6fExJutPA8wDKHqs1NgkHz4P
        Storage size: 2611 bytes
        Updated big_maps:
          New map(1142) of type (big_map nat (pair bool (set nat)))
          New map(1141) of type (big_map address nat)
        Paid storage size diff: 2611 bytes
        Consumed gas: 77578
        Balance updates:
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ2.611
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.257

New contract KT1EYLosB86x6fExJutPA8wDKHqs1NgkHz4P originated.
Contract memorized as Whitelist.
```

# Originating the Whitelisted FA1.2 contract

The CLI interface for the Whitelisted FA1.2 a.k.a. `WhitelistManagedLedger`
only includes the `print` command:

```bash
❯❯❯ ./stack exec -- lorentz-contract-whitelist WhitelistManagedLedger --help

Usage: lorentz-contract-whitelist WhitelistManagedLedger COMMAND
  Whitelist Wrapped ManagedLedger contract CLI interface

Available options:
  -h,--help                Show this help text

Available commands:
  print                    Dump the Oracle contract in form of Michelson code
  init                     Initial storage for the Whitelist Wrapped
                           ManagedLedger contract
```


## Printing the Whitelisted FA1.2 contract

The print command only has arguments to output to a file or print on one line:

```bash
❯❯❯ ./stack exec -- lorentz-contract-whitelist WhitelistManagedLedger print --help

Usage: lorentz-contract-whitelist WhitelistManagedLedger print [-o|--output FILEPATH]
                                                               [--oneline]
  Dump the Oracle contract in form of Michelson code

Available options:
  -h,--help                Show this help text
  -o,--output FILEPATH     File to use as output. If not specified, stdout is
                           used.
  --oneline                Force single line output
  -h,--help                Show this help text
```


## Initial storage

We can use the `init` command from the `WhitelistManagedLedger` part
to generate the initial storage:

```bash
❯❯❯ ./stack exec -- lorentz-contract-whitelist WhitelistManagedLedger init --help

Usage: lorentz-contract-whitelist WhitelistManagedLedger init --issuer ADDRESS
                                                              --users User Whitelists
                                                              --whitelists Whitelists and their allowed outbound Whitelists
                                                              --admin ADDRESS
                                                              --managedLedgerAdmin ADDRESS
                                                              --balances [(ADDRESS, NATURAL)]
  Initial storage for the Whitelist Wrapped ManagedLedger contract

Available options:
  -h,--help                Show this help text
  --issuer ADDRESS         Address of the issuer.
  --users User Whitelists  User Whitelists: User, Whitelist ID or Nothing
  --whitelists Whitelists and their allowed outbound Whitelists
                           Whitelists: Whitelist ID, Restricted, Allowed
                           outbound Whitelist IDs
  --admin ADDRESS          Address of the admin.
  --managedLedgerAdmin ADDRESS
                           Address of the managedLedgerAdmin.
  --balances [(ADDRESS, NATURAL)]
                           The initial balances
  -h,--help                Show this help text
```

We'll need to set:
- `issuer`: A privledged address that may transfer without restrictions
- `users`: A map of user addresses to whitelist ID's
- `whitelists`: A map from whitelist ID's to `(restricted :: Bool, outboundAllowedWhitelists :: [WhitelistId])`
- `admin`: the whitelist admin, that can change the issuer and update the whitelists
- `managedLedgerAdmin`: the admin of the `ManagedLedger` part, who can mint/burn/pause
- `balances`: the initial balances

For testing purposes, we'll set the issuer to a contract that won't be able
to send operations, an already-originated FA1.2 on `babylonnet`: `KT1RUhPAABRhZBctcsWFtymyjpuBQdLTqaAQ`

```bash
❯❯❯ FA12_ADDRESS="KT1RUhPAABRhZBctcsWFtymyjpuBQdLTqaAQ"

  --issuer $FA12_ADDRESS
```

We'll leave the `whitelists`, `users` and `balances` initially empty:

```bash
  --whitelists ""
  --users ""
  --balances "[]"
```

Finally, we'll make `alice` both the whitelist `admin` and the `managedLedgerAdmin`:

```bash
  --admin $ALICE_ADDRESS
  --managedLedgerAdmin $ALICE_ADDRESS
```

```bash
❯❯❯ ./stack exec -- lorentz-contract-whitelist WhitelistManagedLedger init \
  --issuer $FA12_ADDRESS \
  --whitelists "" \
  --users "" \
  --balances "[]" \
  --admin $ALICE_ADDRESS \
  --managedLedgerAdmin $ALICE_ADDRESS

Pair (Pair { } (Pair "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr" (Pair False 0))) (Pair (Pair "KT1RUhPAABRhZBctcsWFtymyjpuBQdLTqaAQ" { }) (Pair { } "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr"))
```


## Running the origination

```bash
❯❯❯ alpha-client --wait none originate contract WhitelistedManagedLedger \
  transferring 0 from $ALICE_ADDRESS running \
  "$(./stack exec -- lorentz-contract-whitelist WhitelistManagedLedger print \
  --oneline)" \
  --init "$(./stack exec -- lorentz-contract-Whitelist WhitelistManagedLedger init \
  --issuer $FA12_ADDRESS \
  --whitelists "" \
  --users "" \
  --balances "[]" \
  --admin $ALICE_ADDRESS \
  --managedLedgerAdmin $ALICE_ADDRESS)" --burn-cap 10.364 

Waiting for the node to be bootstrapped before injection...
Current head: BLm8QfzXh5mw (timestamp: 2020-01-02T23:16:18-00:00, validation: 2020-01-02T23:16:22-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 302470 units (will add 100 for safety)
Estimated storage: 10364 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'ooGzhDKyYqJrFAXyRpo6TbpgBFd3t9r5jfxtWUQFSDsvovGaXWP'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for ooGzhDKyYqJrFAXyRpo6TbpgBFd3t9r5jfxtWUQFSDsvovGaXWP to be included --confirmations 30 --branch BLm8QfzXh5mwW44YwUsQYnk9v2P5uAHsjEhSV5KABgoyGBWBxjr
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
    Fee to the baker: ꜩ0.040544
    Expected counter: 61021
    Gas limit: 302570
    Storage limit: 10384 bytes
    Balance updates:
      tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr ............ -ꜩ0.040544
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,87) ... +ꜩ0.040544
    Origination:
      From: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
      Credit: ꜩ0
      Script:
        { parameter
            (or (or (or (or (pair address (pair address nat)) (pair address nat))
                        (or (pair (pair address address) (contract nat))
                            (or (pair address (contract nat)) (pair unit (contract nat)))))
                    (or (or bool address)
                        (or (pair unit (contract address)) (or (pair address nat) (pair address nat)))))
                (or (or (or address (pair address (option nat)))
                        (or (pair nat (option (pair bool (set nat)))) address))
                    (or (or (pair unit (contract address)) (pair address (contract (option nat))))
                        (or (pair nat (contract (option (pair bool (set nat))))) (pair unit (contract address)))))) ;
          storage
            (pair (pair (big_map address (pair nat (map address nat))) (pair address (pair bool nat)))
                  (pair (pair address (big_map address nat))
                        (pair (big_map nat (pair bool (set nat))) address))) ;
          code { DUP ;
                 CAR ;
                 ..
                     DIP { PAIR } ;
                     PAIR } } }
        Initial storage:
          (Pair (Pair {} (Pair "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr" (Pair False 0)))
                (Pair (Pair "KT1RUhPAABRhZBctcsWFtymyjpuBQdLTqaAQ" {})
                      (Pair {} "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr")))
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1BddbuJHr85iomhaoxxwEEy2o38TdZ1rQS
        Storage size: 10107 bytes
        Updated big_maps:
          New map(1213) of type (big_map nat (pair bool (set nat)))
          New map(1212) of type (big_map address nat)
          New map(1211) of type (big_map address (pair nat (map address nat)))
        Paid storage size diff: 10107 bytes
        Consumed gas: 302470
        Balance updates:
          tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr ... -ꜩ10.107
          tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr ... -ꜩ0.257

New contract KT1BddbuJHr85iomhaoxxwEEy2o38TdZ1rQS originated.
Contract memorized as WhitelistedManagedLedger.
```

Set a `bash` alias for the address:

```bash
❯❯❯ WHITELISTED_FA12="KT1BddbuJHr85iomhaoxxwEEy2o38TdZ1rQS"
```

As mentioned before, the polymorhic `Whitelist` part is used for all but
printing and initializing the `WhitelistManagedLedger` contract.




```bash
❯❯❯ alpha-client --wait none transfer 0 from $ALICE_ADDRESS to $ORACLE_ADDRESS \
  --arg "$(./stack exec -- lorentz-contract-oracle Oracle get-value \
  --callbackContract $NAT_STORAGE_ADDRESS)" --burn-cap 0.000001
```

