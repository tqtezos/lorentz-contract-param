

Use the following method to make the initial storage:

```haskell
initStorageWrappedMultisigContractNat ::
   Natural
-> Natural
-> [PublicKey]
-> ( BigMap Bool ()
   , ((BigMapContract Bool () Natural Natural, Natural), Storage))
initStorageWrappedMultisigContractNat initialNat threshold keys
```

In Haskell, generate for `initialNat = 0`, `threshold = 0`, `keys = []`:

```haskell
print . renderDoc . untypeValue . toVal $
  G.initStorageWrappedMultisigContractNat 0 0 []
putStrLn ("" :: String)
```


Examples for setup:

```bash
alpha-client originate contract WrappedMultisigContractNat for $FRED_ADDRESS transferring 0 from $FRED_ADDRESS running "'$(cat contracts/WrappedMultisigContractNat.tz)'" --dry-run --burn-cap 0.00001 --init "Pair { } (Pair (Pair { DUP; CAR; DIP { CDR }; SWAP; DUP; CAR; DIP { CDR }; DIP { SWAP; PAIR; CAR; NIL operation; PAIR; DUP; CAR; DIP { CDR } }; SWAP; DIP { PAIR }; PAIR } 0) (Pair 0 (Pair 0 { })))"

alpha-client originate contract MultisigManagedLedgerAthens \
  for $FRED_ADDRESS transferring 0 from $FRED_ADDRESS running \
  "$(cat MultisigManagedLedgerAthens.tz)" \
  --init "({}, (($(cat ExplicitBigMapManagedLedgerAthens.tz), Pair {} (Pair (Pair \"$FRED_ADDRESS\" False) (Pair 12 (Left \"$FRED_ADDRESS\")))), (0, (0, []))))" --dry-run --burn-cap 0.00001
```

Originating the contract (dry run):

```bash
$ alpha-client originate contract WrappedMultisigContractNat for $FRED_ADDRESS transferring 0 from $FRED_ADDRESS running "$(cat contracts/WrappedMultisigContractNat.tz)" --dry-run --burn-cap 0.00001 --init "Pair { } (Pair (Pair { DUP; CAR; DIP { CDR }; SWAP; DUP; CAR; DIP { CDR }; DIP { SWAP; PAIR; CAR; NIL operation; PAIR; DUP; CAR; DIP { CDR } }; SWAP; DIP { PAIR }; PAIR } 0) (Pair 0 (Pair 0 { })))"

Waiting for the node to be bootstrapped before injection...
Current head: BMDf3mzoca7A (timestamp: 2019-08-15T14:36:26-00:00, validation: 2019-08-15T14:36:50-00:00)
Node is bootstrapped, ready for injecting operations.
Fatal error:
  The operation will burn ꜩ1.365 which is higher than the configured burn cap (ꜩ0.00001).
   Use `--burn-cap 1.365` to emit this operation.
```


Originating the contract (dry run):

```bash
$ alpha-client originate contract WrappedMultisigContractNat for $FRED_ADDRESS transferring 0 from $FRED_ADDRESS running "$(cat contracts/WrappedMultisigContractNat.tz)" --dry-run --burn-cap 1.365 --init "Pair { } (Pair (Pair { DUP; CAR; DIP { CDR }; SWAP; DUP; CAR; DIP { CDR }; DIP { SWAP; PAIR; CAR; NIL operation; PAIR; DUP; CAR; DIP { CDR } }; SWAP; DIP { PAIR }; PAIR } 0) (Pair 0 (Pair 0 { })))"

Waiting for the node to be bootstrapped before injection...
Current head: BMZmsxRbVfb9 (timestamp: 2019-08-15T14:36:56-00:00, validation: 2019-08-15T14:37:14-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 39585 units (will add 100 for safety)
Estimated storage: 1365 bytes added (will add 20 for safety)
Operation: 0xf412f43e1fa7dc6ba0565c30c85f54e962c02c5ec5cb31c2a50502ed29d53655070000452d024b72d5897f14a02dc1a3b8e012c802cc3dec09e2e125904e0000e0c0ab5b9ebe930058f595f32c5e3f9e57e41d590a086e75b89b9a99ec248da6090000452d024b72d5897f14a02dc1a3b8e012c802cc3df428e3e12585b602e90a00452d024b72d5897f14a02dc1a3b8e012c802cc3d00000000ff000003e402000003df05000764036c0765076503620764036207650362055f035c055f056303670501076507610359036c07650765075e07650362076507610359036c03620765055f036d076507610359036c036203620765036207650362055f035c0502020000037e03210316051f02000000020317072e02000000080320053d036d0342020000035d051f020000002303210316051f02000000020317051f020000000d03210316051f020000000203170342034c051f02000002760743036a0000031303190325072c0200000000020000005507430368010000004a536f6d6520746f6b656e7320776572652073656e7420746f207468697320636f6e7472616374206f757473696465206f66207468652064656661756c7420656e74727920706f696e742e0327034c0321051f0200000002034c051f020000003603210316051f020000000203170321034903540342030c051f020000001603210316051f02000000020317051f0200000002034c034c03210316051f02000000020317051f0200000002034c03190325072c02000000000200000021074303680100000016436f756e7465727320646f206e6f74206d617463682e0327051f0200000002034c03210316051f02000000020317051f02000000d6074303620000034c055202000000c7051f0200000002034c034c072d0200000084072f0200000004034c03200200000074034c051f020000006b034c051f0200000012051f020000000b051f02000000020321034c051f020000000b051f02000000020321034c034c051f02000000020318034c072c020000000203200200000023074307650368036c07070100000010496e76616c69645369676e6174757265030b0327020000002a074307650368036c0707010000001746657765725369676e6174757265735468616e4b657973030b0327034c03190332072c0200000000020000001d07430368010000001251756f72756d206e6f742070726573656e740327072d020000002c074307650368036c07070100000019556e636865636b65645369676e61747572657352656d61696e030b032702000000000320051f020000001703210316051f0200000002031707430362000103120342034c051f0200000002034c072e0200000069051f0200000030034c03210316051f02000000020317051f020000001803210316051f02000000020317034c051f0200000002032103420342032603210316051f02000000020317051f020000001a03210316051f02000000020317051f0200000004034c034203420200000013051f02000000020316034c0342034c053d036d051f020000001803210316051f02000000020317051f0200000002034203420342000000680707020000000007070707020000004903210316051f02000000020317034c03210316051f02000000020317051f0200000019034c03420316053d036d034203210316051f02000000020317034c051f020000000203420342000007070000070700000200000000d8050ed9f3747a5fc2b2e3f9a57c161b229662df4098b2508d15ba99ca9daf962c132376f2daf07a2b64d7cfcb782a8e493bcc161532396266856962ba1a720b
Operation hash is 'op8G484w9hmKgbEm2mdVQhY3MBJTBf2w2iY5pEAj3WFsQAZbhXq'
Simulation result:
  Manager signed operations:
    From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
    Fee to the baker: ꜩ0.00126
    Expected counter: 618722
    Gas limit: 10000
    Storage limit: 0 bytes
    Balance updates:
      tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir ............. -ꜩ0.00126
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,281) ... +ꜩ0.00126
    Revelation of manager public key:
      Contract: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
      Key: edpkvMCwX3MyDg92HckSwFVofR8hcZEjAqrhWJ8SGQgkGjgK1V1gPo
      This revelation was successfully applied
      Consumed gas: 10000
  Manager signed operations:
    From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
    Fee to the baker: ꜩ0.005236
    Expected counter: 618723
    Gas limit: 39685
    Storage limit: 1385 bytes
    Balance updates:
      tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir ............. -ꜩ0.005236
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,281) ... +ꜩ0.005236
    Origination:
      From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
      For: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
      Credit: ꜩ0
      Script:
        { parameter (or unit (pair (pair nat (or nat (pair nat (list key)))) (list (option signature)))) ;
          storage
            (pair (big_map bool unit)
                  (pair (pair (lambda
                                 (pair nat (pair (big_map bool unit) nat))
                                 (pair (list operation) (pair (big_map bool unit) nat)))
                              nat)
                        (pair nat (pair nat (list key))))) ;
          code { DUP ;
                 CAR ;
                 DIP { CDR } ;
                 IF_LEFT
                   { DROP ; NIL operation ; PAIR }
                   { DIP { DUP ;
                           CAR ;
                           DIP { CDR } ;
                           DIP { DUP ; CAR ; DIP { CDR } } ;
                           PAIR } ;
                     SWAP ;
                     DIP { PUSH mutez 0 ;
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
                                                      DIP { CHECK_SIGNATURE } ;
                                                      SWAP ;
                                                      IF { DROP }
                                                         { PUSH (pair string unit) (Pair "InvalidSignature" Unit) ; FAILWITH } } } }
                                          { PUSH (pair string unit) (Pair "FewerSignaturesThanKeys" Unit) ; FAILWITH } ;
                                        SWAP } } ;
                           COMPARE ;
                           LE ;
                           IF {} { PUSH string "Quorum not present" ; FAILWITH } ;
                           IF_CONS
                             { PUSH (pair string unit) (Pair "UncheckedSignaturesRemain" Unit) ; FAILWITH }
                             {} ;
                           DROP ;
                           DIP { DUP ; CAR ; DIP { CDR } ; PUSH nat 1 ; ADD ; PAIR } } ;
                     SWAP ;
                     DIP { SWAP } ;
                     IF_LEFT
                       { DIP { SWAP ;
                               DUP ;
                               CAR ;
                               DIP { CDR } ;
                               DIP { DUP ; CAR ; DIP { CDR } ; SWAP ; DIP { DUP } } ;
                               PAIR } ;
                         PAIR ;
                         EXEC ;
                         DUP ;
                         CAR ;
                         DIP { CDR } ;
                         DIP { DUP ; CAR ; DIP { CDR } ; DIP { SWAP ; PAIR } ; PAIR } }
                       { DIP { CAR } ; SWAP ; PAIR ; SWAP ; NIL operation } ;
                     DIP { DUP ; CAR ; DIP { CDR } ; DIP { PAIR } ; PAIR } ;
                     PAIR } } }
        Initial storage:
          (Pair {}
                (Pair (Pair { DUP ;
                              CAR ;
                              DIP { CDR } ;
                              SWAP ;
                              DUP ;
                              CAR ;
                              DIP { CDR } ;
                              DIP { SWAP ; PAIR ; CAR ; NIL operation ; PAIR ; DUP ; CAR ; DIP { CDR } } ;
                              SWAP ;
                              DIP { PAIR } ;
                              PAIR }
                            0)
                      (Pair 0 (Pair 0 {}))))
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1KooxjeQEK33gSWfoKQZU3JpAxG1KQamKw
        Storage size: 1108 bytes
        Paid storage size diff: 1108 bytes
        Consumed gas: 39585
        Balance updates:
          tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir ... -ꜩ1.108
          tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir ... -ꜩ0.257
```


Originating the contract:

```bash
$ alpha-client originate contract WrappedMultisigContractNat for $FRED_ADDRESS transferring 0 from $FRED_ADDRESS running "$(cat contracts/WrappedMultisigContractNat.tz)" --burn-cap 1.365 --init "Pair { } (Pair (Pair { DUP; CAR; DIP { CDR }; SWAP; DUP; CAR; DIP { CDR }; DIP { SWAP; PAIR; CAR; NIL operation; PAIR; DUP; CAR; DIP { CDR } }; SWAP; DIP { PAIR }; PAIR } 0) (Pair 0 (Pair 0 { })))"

Waiting for the node to be bootstrapped before injection...
Current head: BKq359kz8gtC (timestamp: 2019-08-15T14:37:56-00:00, validation: 2019-08-15T14:38:38-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 39585 units (will add 100 for safety)
Estimated storage: 1365 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'ooGhAT1Q3ALajw8nN9e8LMHumw9YGGExsKbVyveLgRdfUKDi1Vx'
Waiting for the operation to be included...

Fatal error:
  origination simulation failed
```

Find and set the contract's address:

```bash
MULTINAT_ADDRESS="KT1EoTsYYxkH96B99oegxfcgp46AkkPXsNcy" 
```

Test generating parameter to change base contract storage (from `0`) to `7`:

```bash
$ stack build --fast && stack exec -- lorentz-contract-param WrappedMultisigContractNat "MainParameter ((0, Operation 7), [])"
(Right (Pair (Pair 0 (Left 7)) { }))
```

Pass parameter to change base contract storage to `7` (dry run):

```bash
$ alpha-client transfer 0 from $FRED_ADDRESS to $MULTINAT_ADDRESS --arg "$(stack exec -- lorentz-contract-param WrappedMultisigContractNat "MainParameter ((0, Operation 7), [])")" --dry-run --burn-cap 0.00001

Waiting for the node to be bootstrapped before injection...
Current head: BLnNHe7wa12p (timestamp: 2019-08-15T14:44:56-00:00, validation: 2019-08-15T14:45:17-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 37992 units (will add 100 for safety)
Estimated storage: no bytes added
Operation: 0x8cf8f2accd69a55ce50b9bb0a86f70c396268326e53bd548c5c6e8c32f1567ac080000452d024b72d5897f14a02dc1a3b8e012c802cc3df31fe4e125cca90200000144376477a0fed6ae9bd73d9f7f721dd2f41ad66b00ff00000011050807070707000005050007020000000080bc9b4dce8db1f9b87a463d97c883e60f9c06b418174f2a7730e2699fee189f8387c6fb1df1cac5702b252757b02bb9f7488a99b6e53ff8f0bea1a830e0db0a
Operation hash is 'ooreBbQCEGZvoQ579qUwwLTeUXzT6EUnmJ1XRc2PQexgfDTBafH'
Simulation result:
  Manager signed operations:
    From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
    Fee to the baker: ꜩ0.004083
    Expected counter: 618724
    Gas limit: 38092
    Storage limit: 0 bytes
    Balance updates:
      tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir ............. -ꜩ0.004083
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,282) ... +ꜩ0.004083
    Transaction:
      Amount: ꜩ0
      From: tz1RwoEdg4efDQHarsw6aKtMUYvg278Gv1ir
      To: KT1EoTsYYxkH96B99oegxfcgp46AkkPXsNcy
      Parameter: (Right (Pair (Pair 0 (Left 7)) {}))
      This transaction was successfully applied
      Updated storage:
        (Pair {}
              (Pair (Pair { DUP ;
                            CAR ;
                            DIP { CDR } ;
                            SWAP ;
                            DUP ;
                            CAR ;
                            DIP { CDR } ;
                            DIP { SWAP ; PAIR ; CAR ; NIL operation ; PAIR ; DUP ; CAR ; DIP { CDR } } ;
                            SWAP ;
                            DIP { PAIR } ;
                            PAIR }
                          7)
                    (Pair 1 (Pair 0 {}))))
      Storage size: 1108 bytes
      Consumed gas: 37992
```


Pass parameter to change base contract storage to `7`:

```bash
$ alpha-client transfer 0 from $FRED_ADDRESS to $MULTINAT_ADDRESS --arg "$(stack exec -- lorentz-contract-param WrappedMultisigContractNat "MainParameter ((0, Operation 7), [])")" --burn-cap 0.00001

Waiting for the node to be bootstrapped before injection...
Current head: BM98T5V1wZFC (timestamp: 2019-08-15T14:45:26-00:00, validation: 2019-08-15T14:45:34-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 37992 units (will add 100 for safety)
Estimated storage: no bytes added
Operation successfully injected in the node.
Operation hash is 'opPbkkHL2xq5kFdHigcG1RSR5VjrE5JwrXhHTuduC4KY4meTR2N'
Waiting for the operation to be included...

Fatal error:
  transfer simulation failed
```


Get the contract storage to confirm the change:

```bash
$ alpha-client get script storage for $MULTINAT_ADDRESS

Pair {}
     (Pair (Pair { DUP ;
                   CAR ;
                   DIP { CDR } ;
                   SWAP ;
                   DUP ;
                   CAR ;
                   DIP { CDR } ;
                   DIP { SWAP ; PAIR ; CAR ; NIL operation ; PAIR ; DUP ; CAR ; DIP { CDR } } ;
                   SWAP ;
                   DIP { PAIR } ;
                   PAIR }
                 7)
           (Pair 1 (Pair 0 {})))
```

