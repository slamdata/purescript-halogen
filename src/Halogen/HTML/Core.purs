module Halogen.HTML.Core
  ( HTML(..)
  , renderWidget
  , widget
  , text
  , element
  , keyed
  , prop
  , attr
  , handler
  , ref
  , class IsProp
  , toPropValue
  , module Exports
  ) where

import Prelude

import DOM.HTML.Indexed.ButtonType (ButtonType, renderButtonType)
import DOM.HTML.Indexed.CrossOriginValue (CrossOriginValue, renderCrossOriginValue)
import DOM.HTML.Indexed.DirValue (DirValue, renderDirValue)
import DOM.HTML.Indexed.FormMethod (FormMethod, renderFormMethod)
import DOM.HTML.Indexed.InputAcceptType (InputAcceptType, renderInputAcceptType)
import DOM.HTML.Indexed.InputType (InputType, renderInputType)
import DOM.HTML.Indexed.KindValue (KindValue, renderKindValue)
import DOM.HTML.Indexed.MenuType (MenuType, renderMenuType)
import DOM.HTML.Indexed.MenuitemType (MenuitemType, renderMenuitemType)
import DOM.HTML.Indexed.OnOff (OnOff, renderOnOff)
import DOM.HTML.Indexed.OrderedListType (OrderedListType, renderOrderedListType)
import DOM.HTML.Indexed.PreloadValue (PreloadValue, renderPreloadValue)
import DOM.HTML.Indexed.ScopeValue (ScopeValue, renderScopeValue)
import DOM.HTML.Indexed.StepValue (StepValue, renderStepValue)
import DOM.HTML.Indexed.WrapValue (WrapValue, renderWrapValue)
import Data.Bifunctor (class Bifunctor, bimap, rmap)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.Newtype (class Newtype, un, unwrap)
import Data.Tuple (Tuple)
import Halogen.Query.Input (Input)
import Halogen.VDom (ElemName(..), Namespace(..)) as Exports
import Halogen.VDom.DOM.Prop (ElemRef(..), Prop(..), PropValue, propFromBoolean, propFromInt, propFromNumber, propFromString)
import Halogen.VDom.DOM.Prop (Prop(..), PropValue) as Exports
import Halogen.VDom.Types as VDom
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (Element)
import Web.Event.Event (Event, EventType)
import Web.HTML.Common (AttrName(..), ClassName(..), PropName(..)) as Exports
import Web.HTML.Common (AttrName(..), PropName(..))

newtype HTML w i = HTML (VDom.VDom (Array (Prop (Input i))) w)

derive instance newtypeHTML :: Newtype (HTML w i) _

instance bifunctorHTML :: Bifunctor HTML where
  bimap f g (HTML vdom) = HTML (bimap (map (map (map g))) f vdom)

instance functorHTML :: Functor (HTML p) where
  map = rmap

renderWidget :: ∀ w x i j. (i -> j) -> (w -> HTML x j) -> HTML w i -> HTML x j
renderWidget f g (HTML vdom) =
  HTML (VDom.renderWidget (map (map (map f))) (un HTML <<< g) vdom)

widget :: forall p q. p -> HTML p q
widget = HTML <<< VDom.Widget

-- | Constructs a text node `HTML` value.
text :: forall w i. String -> HTML w i
text = HTML <<< VDom.Text

-- | A smart constructor for HTML elements.
element :: forall w i. Maybe VDom.Namespace -> VDom.ElemName -> Array (Prop i) -> Array (HTML w i) -> HTML w i
element ns =
  coe (\name props children -> VDom.Elem ns name props children)
  where
  coe
    :: (VDom.ElemName -> Array (Prop i) -> Array (VDom.VDom (Array (Prop i)) w) -> VDom.VDom (Array (Prop i)) w)
    -> VDom.ElemName -> Array (Prop i) -> Array (HTML w i) -> HTML w i
  coe = unsafeCoerce

-- | A smart constructor for HTML elements with keyed children.
keyed :: forall w i. Maybe VDom.Namespace -> VDom.ElemName -> Array (Prop i) -> Array (Tuple String (HTML w i)) -> HTML w i
keyed ns = coe (\name props children -> VDom.Keyed ns name props children)
  where
  coe
    :: (VDom.ElemName -> Array (Prop i) -> Array (Tuple String (VDom.VDom (Array (Prop i)) w)) -> VDom.VDom (Array (Prop i)) w)
    -> VDom.ElemName -> Array (Prop i) -> Array (Tuple String (HTML w i)) -> HTML w i
  coe = unsafeCoerce

-- | Create a HTML property.
prop :: forall value i. IsProp value => PropName value -> value -> Prop i
prop (PropName name) = Property name <<< toPropValue

-- | Create a HTML attribute.
attr :: forall i. Maybe VDom.Namespace -> AttrName -> String -> Prop i
attr ns (AttrName name) = Attribute ns name

-- | Create an event handler.
handler :: forall i. EventType -> (Event -> Maybe i) -> Prop i
handler = Handler

ref :: forall i. (Maybe Element -> Maybe i) -> Prop i
ref f = Ref $ f <<< case _ of
  Created x -> Just x
  Removed _ -> Nothing

class IsProp a where
  toPropValue :: a -> PropValue

instance isPropString :: IsProp String where
  toPropValue = propFromString

instance isPropInt :: IsProp Int where
  toPropValue = propFromInt

instance isPropNumber :: IsProp Number where
  toPropValue = propFromNumber

instance isPropBoolean :: IsProp Boolean where
  toPropValue = propFromBoolean

instance isPropMediaType :: IsProp MediaType where
  toPropValue = propFromString <<< unwrap

instance isPropButtonType :: IsProp ButtonType where
  toPropValue = propFromString <<< renderButtonType

instance isPropCrossOriginValue :: IsProp CrossOriginValue where
  toPropValue = propFromString <<< renderCrossOriginValue

instance isPropDirValue :: IsProp DirValue where
  toPropValue = propFromString <<< renderDirValue

instance isPropFormMethod :: IsProp FormMethod where
  toPropValue = propFromString <<< renderFormMethod

instance isPropInputType :: IsProp InputType where
  toPropValue = propFromString <<< renderInputType

instance isPropKindValue :: IsProp KindValue where
  toPropValue = propFromString <<< renderKindValue

instance isPropMenuitemType :: IsProp MenuitemType where
  toPropValue = propFromString <<< renderMenuitemType

instance isPropMenuType :: IsProp MenuType where
  toPropValue = propFromString <<< renderMenuType

instance isPropOnOff :: IsProp OnOff where
  toPropValue = propFromString <<< renderOnOff

instance isPropOrderedListType :: IsProp OrderedListType where
  toPropValue = propFromString <<< renderOrderedListType

instance isPropPreloadValue :: IsProp PreloadValue where
  toPropValue = propFromString <<< renderPreloadValue

instance isPropScopeValue :: IsProp ScopeValue where
  toPropValue = propFromString <<< renderScopeValue

instance isPropStepValue :: IsProp StepValue where
  toPropValue = propFromString <<< renderStepValue

instance isPropWrapValue :: IsProp WrapValue where
  toPropValue = propFromString <<< renderWrapValue

instance isPropInputAcceptType :: IsProp InputAcceptType where
  toPropValue = propFromString <<< renderInputAcceptType
