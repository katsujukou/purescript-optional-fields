module Optional.Fields
  ( optional
  , class Optional
  , class OptionalRecordProps
  , class GetTypeWithDefault
  , class DropIfExists
  , class DropIfExistsRowList
  , optionalWithContext
  , optionalRecordProps
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Optional.Fields.Errors (class RenderError)
import Optional.Fields.Types (AtProp, Context, Root, TypeError, UnexpectedType, UnsupportedProp)
import Optional.Fields.Unsafe (unsafeGet)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Prim.TypeError (class Fail)
import Record.Unsafe (unsafeSet)
import Type.Equality (class TypeEquals, from)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

optional :: forall @t a b. Optional (Root ()) t a b => a -> b
optional = optionalWithContext (Proxy @(Root () t))

class Optional :: (Type -> Context) -> Type -> Type -> Type -> Constraint
class Optional ctx t a b | ctx t a -> b where
  optionalWithContext :: Proxy (ctx t) -> a -> b

instance optionalRecord ::
  ( RowToList r l
  , RowToList ri li
  , OptionalRecordProps ctx r l li ri ro
  ) =>
  Optional ctx { | r } { | ri } (Maybe { | ro })
  where
  optionalWithContext p ri = Just (optionalRecordProps (Proxy @l) p ri)

else instance optionalMaybe :: Optional ctx a (Maybe a) (Maybe a) where
  optionalWithContext _ = identity

else instance optionalDefault :: Optional ctx a a (Maybe a) where
  optionalWithContext _ = Just

else instance optionalIncompatible ::
  ( RenderError (TypeError (ctx a) (UnexpectedType a b)) error
  , Fail error
  ) =>
  Optional ctx a b err where
  optionalWithContext _ _ = unsafeCrashWith "optional incompatible"

class OptionalRecordProps :: (Type -> Context) -> Row Type -> RowList Type -> RowList Type -> Row Type -> Row Type -> Constraint
class OptionalRecordProps ctx r l li ri ro | ctx r l ri -> li ro where
  optionalRecordProps :: Proxy l -> Proxy (ctx { | r }) -> Record ri -> Record ro

instance
  ( TypeEquals emp ()
  , TypeEquals (Record emp) {}
  ) =>
  OptionalRecordProps ctx emp RL.Nil RL.Nil emp emp where
  optionalRecordProps _ _ _ = from {}

else instance optionalRecordPropsCons ::
  ( RowToList ri li
  , Row.Cons prop typ rest r
  , RowToList rest tail
  -- , GetTypeWithDefault typ prop li typ'
  , DropIfExists ri prop irest
  , RowToList irest litail
  , OptionalRecordProps ctx rest tail litail irest orest
  , Optional (AtProp ctx prop r) typ (Maybe typ) op
  , Row.Cons prop op orest ro
  , IsSymbol prop
  ) =>
  OptionalRecordProps ctx r (RL.Cons prop typ tail) li ri ro
  where
  optionalRecordProps _ _ ri =
    let
      propProxy = Proxy @prop
      prop = reflectSymbol propProxy
      (a :: Maybe typ) = unsafeGet prop ri
      rest = optionalRecordProps (Proxy @tail) (Proxy @(ctx { | rest })) ((unsafeCoerce ri) :: Record irest)
    in
      unsafeSet prop (optionalWithContext (Proxy @(AtProp ctx prop r (Maybe typ))) a) rest

else instance optionalRecordPropsUnexpected ::
  ( RowToList row (RL.Cons _3 (Record r') _2)
  , RenderError (TypeError (ctx row { | r }) (UnsupportedProp label r')) doc
  , Fail doc
  ) =>
  OptionalRecordProps (ctx row) r RL.Nil (RL.Cons label typ _1) ri ro

-- | Auxiliary class working as typelevel function 
-- | which searches the given label `p` in the given `RowList` `l`.
-- | When the RowList does not contain that label, this type-level
-- | function returns the given default type `d`.
class GetTypeWithDefault :: Type -> Symbol -> RowList Type -> Type -> Constraint
class GetTypeWithDefault d p l t | d p l -> t

instance getTypeWithDefaultNil :: GetTypeWithDefault d p RL.Nil d

else instance getTypeWithDefaulConsHead ::
  GetTypeWithDefault _1 p (RL.Cons p t _2) t

else instance getTypeWithDefaulConsTail ::
  ( GetTypeWithDefault t p cdr r
  ) =>
  GetTypeWithDefault t p (RL.Cons _1 _2 cdr) r

class DropIfExists :: Row Type -> Symbol -> Row Type -> Constraint
class DropIfExists ri label ro | ri label -> ro

instance dropIfExists ::
  ( RowToList ri rl
  , DropIfExistsRowList ri rl label ro
  ) =>
  DropIfExists ri label ro

class DropIfExistsRowList :: Row Type -> RowList Type -> Symbol -> Row Type -> Constraint
class DropIfExistsRowList r rl label ro | r rl label -> ro

instance dropIfExistsRowListFound ::
  ( Row.Cons label _1 ro r
  ) =>
  DropIfExistsRowList r (RL.Cons label _1 _2) label ro

else instance dropIfExistsRowListDone :: DropIfExistsRowList r RL.Nil _1 r

else instance dropIfExistsRowListLoop ::
  ( DropIfExistsRowList r tail label ro
  ) =>
  DropIfExistsRowList r (RL.Cons _1 _2 tail) label ro

type T a =
  { foo :: Int
  , bar :: String
  , baz :: { quz :: a, qux :: Array Boolean }
  }
