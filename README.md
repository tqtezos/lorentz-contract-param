# Lorentz-contract-param

This package contains contracts written on Lorentz - Haskell eDSL for Michelson
contracts, including a Multisig Wrapper contract.

It also contains CLI interfaces for working with Lorentz contracts:
- `lorentz-contract`: list and export supported contracts to Michelson
- `lorentz-contract-param`: generate Michelson contract parameters
- `lorentz-contract-storage`: generate Michelson initial storage values

For more information, see the [quick start guide](https://assets.tqtezos.com/quickstart)
or specifically the [client setup guide](https://assets.tqtezos.com/quickstart/1-index).

Note: As of `09/27/2019` this package will not compile with the latest
`stack`: `2.1.3`.  To build, you'll likely need
[`stack v1.9.3`](https://github.com/commercialhaskell/stack/releases/tag/v1.9.3).

```bash
get_public_key() {tezos-client show address $1 2>/dev/null | tail -n 1 | cut -d " " -f 3}
get_secret_key() {tezos-client show address $1 -S 2>/dev/null | tail -n 1 | cut -d ":" -f 3}
```


```
❯❯❯ ./stack exec -- lorentz-contract print --name "GenericMultisigContract223" --oneline | wc -c
1334

❯❯❯ ./stack exec -- lorentz-contract-storage GenericMultisigContract223 --threshold 1 --signerKeyPairs "[[\"$(get_public_key alice)\", \"$(get_public_key bob)\"]]"                                             $ 
Pair 0 (Pair 1 { Pair "edpkvCHgVArnZo9RTP4P6euLTyhE89u73CYjBgsP4wEJbj4quao9oR" "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb" })
```

```
❮❮❮ ./stack exec -- lorentz-contract-param GenericMultisigContract223-operation --contractAddress $DSTOKEN_ADDRESS --counter 0 --operation "{ DROP; NIL operation }" --signerKeyPairs "[[\"$(get_public_key alice)\", \"$(get_public_key bob)\"]]"
Writing parameter to file: "GenericMultisigContract223_0_9YfhzjTTDiXqiNB9KeuQLYUQNX9oCkTAtfEVM7JYZNeHm1vHM.json"
```

```bash
❯❯❯ alpha-client --wait none originate contract Multisig223 \
  transferring 0 from $ALICE_ADDRESS running \
  "$(./stack exec -- lorentz-contract print --name GenericMultisigContract223)" \
  --init "$(./stack exec -- lorentz-contract-storage GenericMultisigContract223 \
  --threshold 1 --signerKeyPairs \
  "[[\"$(get_public_key alice)\", \"$(get_public_key bob)\"]]")" \
  --burn-cap 1.113
Warning:
  
                 This is NOT the Tezos Mainnet.
  
     The node you are connecting to claims to be running on the
               Tezos Alphanet DEVELOPMENT NETWORK.
          Do NOT use your fundraiser keys on this network.
          Alphanet is a testing network, with free tokens.

Waiting for the node to be bootstrapped before injection...
Current head: BKqBq8AXo3PM (timestamp: 2019-11-27T17:39:46-00:00, validation: 2019-11-27T17:47:56-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 31935 units (will add 100 for safety)
Estimated storage: 1113 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'opapj7fSQ84AJ9bLJJPYQWyRuv5Gyn8w3Qf5Dh9ECAhKwpcc6qA'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for opapj7fSQ84AJ9bLJJPYQWyRuv5Gyn8w3Qf5Dh9ECAhKwpcc6qA to be included --confirmations 30 --branch BKqBq8AXo3PMgQzChWbJe1dVLepnAJdd5dEX1sxmBxigTpNLusT
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
    Fee to the baker: ꜩ0.004332
    Expected counter: 30650
    Gas limit: 32035
    Storage limit: 1133 bytes
    Balance updates:
      tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr ............ -ꜩ0.004332
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,52) ... +ꜩ0.004332
    Origination:
      From: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
      Credit: ꜩ0
      Script:
        { parameter
            (or unit
                (pair (pair nat (or (lambda unit (list operation)) (pair nat (list (pair key key)))))
                      (list (option (pair signature signature))))) ;
          storage (pair nat (pair nat (list (pair key key)))) ;
          code { DUP ;
                 CAR ;
                 DIP { CDR } ;
                 IF_LEFT
                   { DROP ; NIL operation ; PAIR }
                   { PUSH mutez 0 ;
                     AMOUNT ;
                     COMPARE ;
                     EQ ;
                     IF {}
                        { PUSH string
                               "Some tokens were sent to this contract outside of the default entry point." ;
                          FAILWITH } ;
                     SWAP ;
                     DUP ;
                     DIP { SWAP } ;
                     DIP { DUP ;
                           CAR ;
                           DIP { CDR } ;
                           DUP ;
                           SELF ;
                           ADDRESS ;
                           PAIR ;
                           PACK ;
                           DIP { DUP ; CAR ; DIP { CDR } ; DIP { SWAP } } ;
                           SWAP } ;
                     DUP ;
                     CAR ;
                     DIP { CDR } ;
                     DIP { SWAP } ;
                     COMPARE ;
                     EQ ;
                     IF {} { PUSH string "Counters do not match." ; FAILWITH } ;
                     DIP { SWAP } ;
                     DUP ;
                     CAR ;
                     DIP { CDR } ;
                     DIP { PUSH nat 0 ;
                           SWAP ;
                           ITER { DIP { SWAP } ;
                                  SWAP ;
                                  IF_CONS
                                    { IF_NONE
                                        { SWAP ; DROP }
                                        { SWAP ;
                                          DIP { SWAP ;
                                                DIP { DIP { DIP { DUP } ; SWAP } } ;
                                                DIP { DIP { DUP } ; SWAP } ;
                                                SWAP ;
                                                DIP { PAIR ;
                                                      DUP ;
                                                      DIP { DUP ;
                                                            CAR ;
                                                            DIP { CDR } ;
                                                            CAR ;
                                                            DIP { CAR ; DIP { DUP } } ;
                                                            CHECK_SIGNATURE } ;
                                                      SWAP ;
                                                      DIP { DUP ; CAR ; DIP { CDR } ; CDR ; DIP { CDR } ; CHECK_SIGNATURE } ;
                                                      AND } ;
                                                SWAP ;
                                                IF { DROP } { FAILWITH } ;
                                                PUSH nat 1 ;
                                                ADD } } }
                                    { FAILWITH } ;
                                  SWAP } } ;
                     COMPARE ;
                     LE ;
                     IF {} { PUSH string "Quorum not present" ; FAILWITH } ;
                     IF_CONS { FAILWITH } {} ;
                     DROP ;
                     DIP { DUP ; CAR ; DIP { CDR } ; PUSH nat 1 ; ADD ; PAIR } ;
                     IF_LEFT
                       { SWAP ; DIP { UNIT ; EXEC } ; SWAP }
                       { DIP { CAR } ; SWAP ; PAIR ; NIL operation } ;
                     PAIR } } }
        Initial storage:
          (Pair 0
                (Pair 1
                      { Pair "edpkvCHgVArnZo9RTP4P6euLTyhE89u73CYjBgsP4wEJbj4quao9oR"
                             "edpkuPTVBFtbYd6gZWryXypSYYq6g7FvyucwphoU78T1vmGkbhj6qb" }))
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1HgbHu8EaK8oKL8JoSmjVBknoMWK4VYyxP
        Storage size: 856 bytes
        Paid storage size diff: 856 bytes
        Consumed gas: 31935
        Balance updates:
          tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr ... -ꜩ0.856
          tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr ... -ꜩ0.257

New contract KT1HgbHu8EaK8oKL8JoSmjVBknoMWK4VYyxP originated.
Contract memorized as Multisig223.
```

Make an alias for the contract's address:

```bash
❯❯❯ MULTISIG223_ADDRESS="KT1HgbHu8EaK8oKL8JoSmjVBknoMWK4VYyxP"
❯❯❯ echo $MULTISIG223_ADDRESS
KT1HgbHu8EaK8oKL8JoSmjVBknoMWK4VYyxP
```

Make a new parameter file:


```bash
❯❯❯ ./stack exec -- lorentz-contract-param GenericMultisigContract223-operation \
  --contractAddress $MULTISIG223_ADDRESS --counter 0 --operation \
  "{ DROP; NIL operation }" --signerKeyPairs \
  "[[\"$(get_public_key alice)\", \"$(get_public_key bob)\"]]"

Writing parameter to file: "GenericMultisigContract223_0_9YfhzjTTDiXqiNB9KeuQLYUQNX9oCkTAtfEVM7JYZNeHm1vHM.json"
```

The parameter is `{ DROP; NIL operation }`: a lambda that does nothing.

Make an alias for the parameter file:

```bash
MULTISIG_PARAMETER_FILE="GenericMultisigContract223_0_9YfhzjTTDiXqiNB9KeuQLYUQNX9oCkTAtfEVM7JYZNeHm1vHM.json"
```

Then `alice` and `bob` can sign the file (in any order):

```bash
❯❯❯ ./stack exec -- lorentz-contract-param MultisigSignFile \
  --secretKey "$(get_secret_key alice)" --publicKey "[\"$(get_public_key alice)\", \"$(get_public_key bob)\"]" --signerFile $MULTISIG_PARAMETER_FILE
Writing parameter to file: "GenericMultisigContract223_0_9YfhzjTTDiXqiNB9KeuQLYUQNX9oCkTAtfEVM7JYZNeHm1vHM.json"

❯❯❯ ./stack exec -- lorentz-contract-param MultisigSignFile \
  --secretKey "$(get_secret_key bob)" --publicKey "[\"$(get_public_key alice)\", \"$(get_public_key bob)\"]" --signerFile $MULTISIG_PARAMETER_FILE
Writing parameter to file: "GenericMultisigContract223_0_9YfhzjTTDiXqiNB9KeuQLYUQNX9oCkTAtfEVM7JYZNeHm1vHM.json"
```

Attempting to re-sign the file fails:

```bash
❯❯❯ ./stack exec -- lorentz-contract-param MultisigSignFile \
  --secretKey "$(get_secret_key bob)" --publicKey "[\"$(get_public_key alice)\", \"$(get_public_key bob)\"]" --signerFile $MULTISIG_PARAMETER_FILE
Writing parameter to file: "GenericMultisigContract223_0_9YfhzjTTDiXqiNB9KeuQLYUQNX9oCkTAtfEVM7JYZNeHm1vHM.json"
signMultisigSignersFile: expected partial signature but got complete signature
CallStack (from HasCallStack):
  error, called at param-app/Multisig.hs:276:19 in main:Multisig
```

Finally, the signed file may be submitted to the contract:

```bash
❯❯❯ alpha-client --wait none transfer 0 from $ALICE_ADDRESS to $MULTISIG223_ADDRESS \
 --arg "$(cat $MULTISIG_PARAMETER_FILE | ./stack exec -- lorentz-contract-param \
 MultisigSignersFile --signerFiles "")" --burn-cap 0.000001
```

