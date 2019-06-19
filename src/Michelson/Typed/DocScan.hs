-- | Extracting documentation from instructions set.
module Michelson.Typed.DocScan
  ( buildInstrDoc
  , modifyInstrDoc
  ) where

import Control.Lens (at)
import Data.Typeable (cast)
import Prelude hiding (Ordering(..))

import Michelson.Typed.Doc
import Michelson.Typed.Instr

someDocItemToBlock :: SomeDocItem -> DocBlock
someDocItemToBlock sdi@(SomeDocItem (_ :: di)) =
  one (docItemPosition @di, one sdi)

someDefinitionDocItemToContractDoc :: SomeDocItem -> DocItemId -> State ContractDoc ()
someDefinitionDocItemToContractDoc sdi docItemId =
  modify $ flip mappend
    mempty
    { cdContents = mempty
    , cdDefinitions = someDocItemToBlock sdi
    , cdDefinitionIds = one docItemId
    }

someDocItemToContractDoc :: SomeDocItem -> State ContractDoc ()
someDocItemToContractDoc sdi@(SomeDocItem di) = do
  () <- case docItemRef di of
    DocItemNoRef ->
      modify (<> mempty{ cdContents = someDocItemToBlock sdi })
    DocItemRef docItemId ->
      someDefinitionDocItemToContractDoc sdi docItemId
  forM_ @_ @_ @() (docItemDependencies di) $ \(SomeDocDefinitionItem dep) ->
    case docItemRef dep of
      DocItemRef docItemId -> do
        -- Taking special treatment for possible cyclic dependencies.
        isPresent <- use $ cdDefinitionIdsL . at docItemId
        case isPresent of
          Just () -> pass
          Nothing -> someDocItemToContractDoc (SomeDocItem dep)

-- | Assemble contract documentation.
buildInstrDoc :: Instr inp out -> ContractDoc
buildInstrDoc = \case
  Seq i1 i2 ->
    buildInstrDoc i1 <> buildInstrDoc i2
  Nop ->
    mempty
  Ext ext -> case ext of
    TEST_ASSERT{} ->
      mempty
    PRINT{} ->
      mempty
    DOC_ITEM sdi ->
      execState (someDocItemToContractDoc sdi) mempty
  Nested i ->
    buildInstrDoc i
  DocGroup grouping code ->
    let inner = buildInstrDoc code
    in inner{ cdContents = someDocItemToBlock . grouping $ cdContents inner }

  DROP -> mempty
  DUP -> mempty
  SWAP -> mempty
  PUSH{} -> mempty
  SOME -> mempty
  NONE -> mempty
  UNIT -> mempty
  IF_NONE l r -> buildInstrDoc l <> buildInstrDoc r
  PAIR -> mempty
  CAR -> mempty
  CDR -> mempty
  LEFT -> mempty
  RIGHT -> mempty
  IF_LEFT l r -> buildInstrDoc l <> buildInstrDoc r
  NIL -> mempty
  CONS -> mempty
  IF_CONS l r -> buildInstrDoc l <> buildInstrDoc r
  SIZE -> mempty
  EMPTY_SET -> mempty
  EMPTY_MAP -> mempty
  MAP i -> buildInstrDoc i
  ITER i -> buildInstrDoc i
  MEM -> mempty
  GET -> mempty
  UPDATE -> mempty
  IF l r -> buildInstrDoc l <> buildInstrDoc r
  LOOP i -> buildInstrDoc i
  LOOP_LEFT i -> buildInstrDoc i
  LAMBDA _l -> mempty
  EXEC -> mempty
  DIP i -> buildInstrDoc i
  FAILWITH -> mempty
  CAST -> mempty
  RENAME -> mempty
  UNPACK -> mempty
  PACK -> mempty
  CONCAT -> mempty
  CONCAT' -> mempty
  SLICE -> mempty
  ISNAT -> mempty
  ADD -> mempty
  SUB -> mempty
  MUL -> mempty
  EDIV -> mempty
  ABS -> mempty
  NEG -> mempty
  LSL -> mempty
  LSR -> mempty
  OR -> mempty
  AND -> mempty
  XOR -> mempty
  NOT -> mempty
  COMPARE -> mempty
  EQ -> mempty
  NEQ -> mempty
  LT -> mempty
  GT -> mempty
  LE -> mempty
  GE -> mempty
  INT -> mempty
  SELF -> mempty
  CONTRACT _ -> mempty
  TRANSFER_TOKENS -> mempty
  SET_DELEGATE -> mempty
  CREATE_ACCOUNT -> mempty
  CREATE_CONTRACT _code -> mempty
  --- ^ it's not entirely clear for now how do we want to handle this case
  IMPLICIT_ACCOUNT -> mempty
  NOW -> mempty
  AMOUNT -> mempty
  BALANCE -> mempty
  CHECK_SIGNATURE -> mempty
  SHA256 -> mempty
  SHA512 -> mempty
  BLAKE2B -> mempty
  HASH_KEY -> mempty
  STEPS_TO_QUOTA -> mempty
  SOURCE -> mempty
  SENDER -> mempty
  ADDRESS -> mempty

-- | Modify all documentation items recursively.
modifyInstrAllDoc
  :: (SomeDocItem -> SomeDocItem)
  -> Instr inp out
  -> Instr inp out
modifyInstrAllDoc mapper = go
  where
  go :: Instr i o -> Instr i o
  go = \case
    Seq i1 i2 -> go i1 `Seq` go i2
    i@Nop -> i
    Ext ext -> Ext $
      case ext of
        i@TEST_ASSERT{} -> i
        i@PRINT{} -> i
        DOC_ITEM sdi -> DOC_ITEM (mapper sdi)
    Nested i -> Nested $ go i
    DocGroup grouping code -> DocGroup grouping $ go code

    i@DROP -> i
    i@DUP -> i
    i@SWAP -> i
    i@PUSH{} -> i
    i@SOME -> i
    i@NONE -> i
    i@UNIT -> i
    IF_NONE l r -> IF_NONE (go l) (go r)
    i@PAIR -> i
    i@CAR -> i
    i@CDR -> i
    i@LEFT -> i
    i@RIGHT -> i
    IF_LEFT l r -> IF_LEFT (go l) (go r)
    i@NIL -> i
    i@CONS -> i
    IF_CONS l r -> IF_CONS (go l) (go r)
    i@SIZE -> i
    i@EMPTY_SET -> i
    i@EMPTY_MAP -> i
    MAP i -> MAP (go i)
    ITER i -> ITER (go i)
    i@MEM -> i
    i@GET -> i
    i@UPDATE -> i
    IF l r -> IF (go l) (go r)
    LOOP i -> LOOP (go i)
    LOOP_LEFT i -> LOOP_LEFT (go i)
    i@LAMBDA{} -> i
    i@EXEC -> i
    DIP i -> DIP (go i)
    i@FAILWITH -> i
    i@CAST -> i
    i@RENAME -> i
    i@UNPACK -> i
    i@PACK -> i
    i@CONCAT -> i
    i@CONCAT' -> i
    i@SLICE -> i
    i@ISNAT -> i
    i@ADD -> i
    i@SUB -> i
    i@MUL -> i
    i@EDIV -> i
    i@ABS -> i
    i@NEG -> i
    i@LSL -> i
    i@LSR -> i
    i@OR -> i
    i@AND -> i
    i@XOR -> i
    i@NOT -> i
    i@COMPARE -> i
    i@EQ -> i
    i@NEQ -> i
    i@LT -> i
    i@GT -> i
    i@LE -> i
    i@GE -> i
    i@INT -> i
    i@SELF -> i
    i@CONTRACT{} -> i
    i@TRANSFER_TOKENS -> i
    i@SET_DELEGATE -> i
    i@CREATE_ACCOUNT -> i
    i@CREATE_CONTRACT{} -> i
    --- ^ it's not entirely clear for now how do we want to handle this case
    i@IMPLICIT_ACCOUNT -> i
    i@NOW -> i
    i@AMOUNT -> i
    i@BALANCE -> i
    i@CHECK_SIGNATURE -> i
    i@SHA256 -> i
    i@SHA512 -> i
    i@BLAKE2B -> i
    i@HASH_KEY -> i
    i@STEPS_TO_QUOTA -> i
    i@SOURCE -> i
    i@SENDER -> i
    i@ADDRESS -> i

-- | Recursevly traverse an instruction and modify documentation items
-- matching given type.
modifyInstrDoc
  :: DocItem i
  => (i -> i)
  -> Instr inp out
  -> Instr inp out
modifyInstrDoc mapper = modifyInstrAllDoc untypedMapper
  where
  untypedMapper sdi@(SomeDocItem di) = maybe sdi (SomeDocItem . mapper) (cast di)
