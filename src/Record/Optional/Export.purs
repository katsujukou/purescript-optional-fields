module Record.Optional.Export where

import Prelude

import Data.Maybe (Maybe, maybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Foreign (Foreign)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Record.Unsafe (unsafeSet)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class Export a where
  export :: a -> Foreign

instance exportInt :: Export Int where
  export = unsafeCoerce

instance exportNumber :: Export Number where
  export = unsafeCoerce

instance exportString :: Export String where
  export = unsafeCoerce

instance exportBoolean :: Export Boolean where
  export = unsafeCoerce

instance exportArray :: Export a => Export (Array a) where
  export = map export >>> unsafeCoerce

instance exportMaybe :: Export a => Export (Maybe a) where
  export = maybe jsNull export

instance exportRecord ::
  ( RowToList r rl
  , ExportRecordProps r rl
  ) =>
  Export (Record r) where
  export = exportRecordProps (Proxy @rl) >>> unsafeCoerce

class ExportRecordProps :: Row Type -> RowList Type -> Constraint
class ExportRecordProps r rl where
  exportRecordProps :: forall rout. Proxy rl -> Record r -> Record rout

instance exportRecordPropsNil :: ExportRecordProps r RL.Nil where
  exportRecordProps _ _ = unsafeCoerce {}

else instance exportRecordPropsCons ::
  ( Row.Cons prop typ rest rin
  , RowToList rest tail
  , Export typ
  , ExportRecordProps rest tail
  , Row.Lacks prop rest
  , IsSymbol prop
  ) =>
  ExportRecordProps rin (RL.Cons prop typ tail) where
  exportRecordProps _ ri =
    let
      prop = Proxy @prop
      a = export (Record.get prop ri)
      irest = Record.delete prop ri
      orest = exportRecordProps (Proxy @tail) irest
    in
      if isNull a then orest
      else unsafeSet (reflectSymbol prop) a orest

foreign import jsNull :: Foreign

foreign import isNull :: Foreign -> Boolean
