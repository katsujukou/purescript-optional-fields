module Optional.Fields.Unsafe
  ( unsafeGet
  ) where

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe)

foreign import unsafeGet_
  :: forall r b
   . Fn2
       String
       { | r }
       (Maybe b)

unsafeGet :: forall r b. String -> Record r -> Maybe b
unsafeGet = runFn2 unsafeGet_