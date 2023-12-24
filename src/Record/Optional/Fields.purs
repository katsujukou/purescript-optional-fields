module Record.Optional.Fields
  ( optional
  , class Optional
  , class GetTypeWithDefault
  , class OptionalRecordProps
  , class DropIfExists
  , class DropIfExistsRowList
  , optionalWithContext
  , optionalRecordProps
  , getWithDefault
  ) where

import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Lacks)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Prim.TypeError (class Fail)
import Record as Record
import Record.Optional.Fields.Errors (class RenderError)
import Record.Optional.Fields.Types (AtProp, Context, Root, TypeError, UnexpectedType, UnsupportedProp)
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

else instance optionalFlattenRight ::
  ( Optional ctx r a b
  ) =>
  Optional ctx r (Maybe a) b where
  optionalWithContext _ Nothing = unsafeCoerce Nothing
  optionalWithContext p (Just a) = optionalWithContext p a

else instance optionalFlattenLeft ::
  ( Optional ctx r a b
  ) =>
  Optional ctx (Maybe r) a b where
  optionalWithContext _ a = optionalWithContext (Proxy @(ctx r)) a

else instance optionalMaybeBoth ::
  ( Optional ctx r a b
  ) =>
  Optional ctx (Maybe r) (Maybe a) b where
  optionalWithContext _ Nothing = unsafeCoerce Nothing
  optionalWithContext _ (Just a) = optionalWithContext (Proxy @(ctx r)) a

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
  , DropIfExists ri prop irest
  , RowToList irest litail
  , OptionalRecordProps ctx rest tail litail irest orest
  , GetTypeWithDefault ctx (Maybe typ) prop ri li typ'
  -- , TypeEquals typ' (Mayb:re typ)
  , Row.Cons prop typ' orest ro
  , IsSymbol prop
  ) =>
  OptionalRecordProps ctx r (RL.Cons prop typ tail) li ri ro
  where
  optionalRecordProps _ _ ri =
    let
      propProxy = Proxy @prop
      prop = reflectSymbol propProxy
      a = getWithDefault (Proxy @(ctx (Maybe typ))) (Proxy @li) ((unsafeCoerce Nothing) :: Maybe typ) (Proxy @prop) ri
      rest = optionalRecordProps (Proxy @tail) (Proxy @(ctx { | rest })) ((unsafeCoerce ri) :: Record irest)
    in
      unsafeSet prop a rest

else instance optionalRecordPropsUnexpected ::
  ( RowToList row (RL.Cons _3 (Record r') _2)
  , RenderError (TypeError (ctx row { | r }) (UnsupportedProp label r')) doc
  , Fail doc
  ) =>
  OptionalRecordProps (ctx row) r RL.Nil (RL.Cons label typ _1) ri ro

class GetTypeWithDefault :: (Type -> Context) -> Type -> Symbol -> Row Type -> RowList Type -> Type -> Constraint
class GetTypeWithDefault ctx t prop row rl out | ctx t prop row rl -> out where
  getWithDefault :: Proxy (ctx t) -> Proxy rl -> t -> Proxy prop -> { | row } -> out

instance getTypeWithDefaultNil ::
  ( TypeEquals row ()
  , Optional ctx t t out
  ) =>
  GetTypeWithDefault ctx t prop row RL.Nil out
  where
  getWithDefault _ _ def _ _ = optionalWithContext (Proxy @(ctx t)) def

else instance getTypeWithDefaultConsHead ::
  ( Row.Cons prop typ rest row
  , IsSymbol prop
  , Optional (AtProp ctx prop row) t typ out
  ) =>
  GetTypeWithDefault ctx t prop row (RL.Cons prop typ tail) out
  where
  getWithDefault _ _ _ prop ri = optionalWithContext (Proxy @(AtProp ctx prop row t)) (Record.get prop ri)

else instance getTypeWithDefaulConsTail ::
  ( Row.Cons _1 _2 rest row
  , RowToList rest cdr
  , GetTypeWithDefault ctx t prop rest cdr out
  , IsSymbol _1
  , Lacks _1 rest
  ) =>
  GetTypeWithDefault ctx t prop row (RL.Cons _1 _2 cdr) out
  where
  getWithDefault pctx _ def p ri =
    let
      rest = Record.delete (Proxy @_1) ri
    in
      getWithDefault pctx (Proxy @cdr) def p rest

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