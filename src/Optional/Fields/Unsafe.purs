module Optional.Fields.Unsafe
  ( unsafeGet
  ) where

import Data.Function.Uncurried (Fn4, runFn4)
import Data.Maybe (Maybe(..))

foreign import unsafeGet_
  :: forall r b
   . Fn4
       (forall a. Maybe a)
       (forall a. a -> Maybe a)
       String
       { | r }
       (Maybe b)

unsafeGet :: forall r b. String -> Record r -> Maybe b
unsafeGet = runFn4 unsafeGet_ Nothing Just
