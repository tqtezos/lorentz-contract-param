module Morley.Nop
  ( typeCheckMorleyContract
  , nopHandler
  ) where

import Michelson.TypeCheck
import Michelson.Typed (converge, extractNotes)
import Michelson.Untyped (InstrAbstract(..))
import Morley.Types

typeCheckMorleyContract :: Contract (Instr NopInstr) -> Either (TCError NopInstr) SomeContract
typeCheckMorleyContract = typeCheckContract nopHandler

nopHandler :: TcNopHandler NopInstr
nopHandler nop@(STACKTYPE s) si@(SomeIT it) =
  either (Left . TCFailedOnInstr (NOP nop) si) (const $ Right ()) $
  handleStackTypeInstr it 0 s it
nopHandler  _ _ = pure ()

handleStackTypeInstr :: IT initial -> Int -> StackTypePattern -> IT xs -> Either Text ()
handleStackTypeInstr _ _ StkRest _ = pure ()
handleStackTypeInstr _ _ StkEmpty INil = pure ()
handleStackTypeInstr origin i StkEmpty _ =
  Left $ "Unexpected length of the stack: STACKTYPE pattern has length " <> show i <>
        ", but actual stack is longer: " <> show origin <> " elements"
handleStackTypeInstr origin i _ INil =
  Left $ "Unexpected length of the stack: actual stack " <> show origin <> " has length " <> show i <>
        ", but STACKTYPE pattern is longer"
handleStackTypeInstr orig i (StkCons t ts) ((xt, xann, _) ::& xs) = do
  tann <- either (const $ Left $ "Couldn't match " <> show i <> " type of stack")
                pure
                (extractNotes t xt)
  either (const $ Left $ "Couldn't c onverge annotations of " <> show i <> " element of pattern and actual stack")
         (const $ Right ())
         (converge tann xann)
  handleStackTypeInstr orig (i + 1) ts xs
