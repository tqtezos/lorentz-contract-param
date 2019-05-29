-- | Contract which remembers all parameters it has been called with.
--
-- Useful to save return values of @View@ entry points.
module Lorentz.Contracts.Consumer
  ( contractConsumer
  ) where

import Lorentz

-- We hide this constract from the registry because polymorphic contracts
-- are not so easy to untype. And it is used mainly in tests.
-- | Remembers parameters it was called with, last go first.
contractConsumer :: Contract cp [cp]
contractConsumer = do unpair; cons; nil; pair
