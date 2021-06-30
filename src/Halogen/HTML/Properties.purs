-- | A closed signature of type-indexed (refined) HTML properties; these can be
-- | used to ensure correctness by construction, and then erased into the
-- | standard unrefined versions.
module Halogen.HTML.Properties
  ( IProp(..)
  , prop
  , attr
  , attrNS
  , ref
  , expand

  , alt
  , charset
  , class_, classes
  , cols
  , rows
  , colSpan
  , rowSpan
  , for
  , height
  , width
  , href
  , id
  , id_
  , name
  , rel
  , src
  , style
  , scope
  , target
  , title
  , download

  , method
  , action
  , enctype
  , noValidate

  , type_
  , value
  , min
  , max
  , step
  , disabled
  , enabled
  , required
  , readOnly
  , spellcheck
  , checked
  , selected
  , selectedIndex
  , placeholder
  , autocomplete
  , list
  , autofocus
  , multiple
  , pattern
  , accept

  , autoplay
  , controls
  , loop
  , muted
  , poster
  , preload

  , draggable
  , tabIndex

  , module I
  ) where

import Prelude

import DOM.HTML.Indexed (CSSPixel) as I
import DOM.HTML.Indexed.ButtonType (ButtonType(..)) as I
import DOM.HTML.Indexed.FormMethod (FormMethod(..)) as I
import DOM.HTML.Indexed.InputAcceptType (InputAcceptType(..)) as I
import DOM.HTML.Indexed.InputType (InputType(..)) as I
import DOM.HTML.Indexed.MenuType (MenuType(..)) as I
import DOM.HTML.Indexed.MenuitemType (MenuitemType(..)) as I
import DOM.HTML.Indexed.OnOff (OnOff(..)) as I
import DOM.HTML.Indexed.OrderedListType (OrderedListType(..)) as I
import DOM.HTML.Indexed.PreloadValue (PreloadValue(..)) as I
import DOM.HTML.Indexed.ScopeValue (ScopeValue(..)) as I
import DOM.HTML.Indexed.StepValue (StepValue(..)) as I
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.Newtype (class Newtype)
import Data.String (joinWith)
import Halogen.HTML.Core (class IsProp, AttrName(..), ClassName, Namespace, PropName(..), Prop)
import Halogen.HTML.Core as Core
import Halogen.Query.Input (Input(..), RefLabel)
import Prim.Row as Row
import Prim.TypeError as TypeError
import Safe.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (Element)

-- | The phantom row `r` can be thought of as a context which is synthesized in
-- | the course of constructing a refined HTML expression.
newtype IProp (r :: Row Type) i = IProp (Prop (Input i))

derive instance newtypeIProp :: Newtype (IProp r i) _
derive instance functorIProp :: Functor (IProp r)

-- | Creates an indexed HTML property.
prop
  :: forall value r i
   . IsProp value
  => PropName value
  -> value
  -> IProp r i
prop = (unsafeCoerce :: (PropName value -> value -> Prop (Input i)) -> PropName value -> value -> IProp r i) Core.prop

-- | Creates an indexed HTML attribute.
attr :: forall r i. AttrName -> String -> IProp r i
attr =
  Core.attr Nothing #
    (unsafeCoerce
      :: (AttrName -> String -> Prop (Input i))
      -> AttrName
      -> String
      -> IProp r i)

-- | Creates an indexed HTML attribute.
attrNS :: forall r i. Namespace -> AttrName -> String -> IProp r i
attrNS =
  pure >>> Core.attr >>>
    (unsafeCoerce
      :: (AttrName -> String -> Prop (Input i))
      -> AttrName
      -> String
      -> IProp r i)

-- | The `ref` property allows an input to be raised once a `HTMLElement` has
-- | been created or destroyed in the DOM for the element that the property is
-- | attached to.
ref :: forall r i. RefLabel -> IProp r i
ref = (unsafeCoerce :: ((Maybe Element -> Maybe (Input i)) -> Prop (Input i)) -> (Maybe Element -> Maybe (Input i)) -> IProp r i) Core.ref <<< go
  where
  go :: RefLabel -> Maybe Element -> Maybe (Input i)
  go p mel = Just (RefUpdate p mel)

-- | Every `IProp lt i` can be cast to some `IProp gt i` as long as `lt` is a
-- | subset of `gt`.
expand ∷ ∀ lt gt a i. Row.Union lt a gt ⇒ IProp lt i → IProp gt i
expand = unsafeCoerce

alt :: forall r i. String -> IProp (alt :: String | r) i
alt = prop (PropName "alt")

charset :: forall r i. String -> IProp (charset :: String | r) i
charset = prop (PropName "charset")

class_ :: forall r i. ClassName -> IProp (class :: String | r) i
class_ = prop (PropName "className") <<< (coerce :: ClassName -> String)

classes :: forall r i. Array ClassName -> IProp (class :: String | r) i
classes = prop (PropName "className") <<< joinWith " " <<< (coerce :: Array ClassName -> Array String)

cols :: forall r i. Int -> IProp (cols :: Int | r) i
cols = prop (PropName "cols")

rows :: forall r i. Int -> IProp (rows :: Int | r) i
rows = prop (PropName "rows")

colSpan :: forall r i. Int -> IProp (colSpan :: Int | r) i
colSpan = prop (PropName "colSpan")

rowSpan :: forall r i. Int -> IProp (rowSpan :: Int | r) i
rowSpan = prop (PropName "rowSpan")

for :: forall r i. String -> IProp (for :: String | r) i
for = prop (PropName "htmlFor")

height :: forall r i. I.CSSPixel -> IProp (height :: I.CSSPixel | r) i
height = prop (PropName "height")

width :: forall r i. I.CSSPixel -> IProp (width :: I.CSSPixel | r) i
width = prop (PropName "width")

href :: forall r i. String -> IProp (href :: String | r) i
href = prop (PropName "href")

id :: forall r i. String -> IProp (id :: String | r) i
id = prop (PropName "id")

id_ :: forall r i. TypeError.Warn (TypeError.Text "`id_` is deprecated. Use `id` instead.") => String -> IProp (id :: String | r) i
id_ = id

name :: forall r i. String -> IProp (name :: String | r) i
name = prop (PropName "name")

rel :: forall r i. String -> IProp (rel :: String | r) i
rel = prop (PropName "rel")

src :: forall r i. String -> IProp (src :: String | r) i
src = prop (PropName "src")

-- | Sets the `style` attribute to the specified string.
-- |
-- | ```purs
-- | ... [ style "height: 50px;" ]
-- | ```
-- |
-- | If you prefer to use typed CSS for this attribute, you can use the purescript-halogen-css library:
-- | https://github.com/purescript-halogen/purescript-halogen-css
style :: forall r i. String -> IProp (style :: String | r) i
style = attr (AttrName "style")

scope :: forall r i. I.ScopeValue -> IProp (scope :: I.ScopeValue | r) i
scope = prop (PropName "scope")

target :: forall r i. String -> IProp (target :: String | r) i
target = prop (PropName "target")

title :: forall r i. String -> IProp (title :: String | r) i
title = prop (PropName "title")

download :: ∀ r i. String -> IProp (download :: String | r) i
download = prop (PropName "download")

method :: forall r i. I.FormMethod -> IProp (method :: I.FormMethod | r) i
method = prop (PropName "method")

action :: forall r i. String -> IProp (action :: String | r) i
action = prop (PropName "action")

enctype :: forall r i. MediaType -> IProp (enctype :: MediaType | r) i
enctype = prop (PropName "enctype")

noValidate :: forall r i. Boolean -> IProp (noValidate :: Boolean | r) i
noValidate = prop (PropName "noValidate")

type_ :: forall r i value. IsProp value => value -> IProp (type :: value | r) i
type_ = prop (PropName "type")

value :: forall r i. String -> IProp (value :: String | r) i
value = prop (PropName "value")

min :: forall r i. Number -> IProp (min :: Number | r) i
min = prop (PropName "min")

max :: forall r i. Number -> IProp (max :: Number | r) i
max = prop (PropName "max")

step :: forall r i. I.StepValue -> IProp (step :: I.StepValue | r) i
step = prop (PropName "step")

enabled :: forall r i. Boolean -> IProp (disabled :: Boolean | r) i
enabled = disabled <<< not

disabled :: forall r i. Boolean -> IProp (disabled :: Boolean | r) i
disabled = prop (PropName "disabled")

required :: forall r i. Boolean -> IProp (required :: Boolean | r) i
required = prop (PropName "required")

readOnly :: forall r i. Boolean -> IProp (readOnly :: Boolean | r) i
readOnly = prop (PropName "readOnly")

spellcheck :: forall r i. Boolean -> IProp (spellcheck :: Boolean | r) i
spellcheck = prop (PropName "spellcheck")

checked :: forall r i. Boolean -> IProp (checked :: Boolean | r) i
checked = prop (PropName "checked")

selected :: forall r i. Boolean -> IProp (selected :: Boolean | r) i
selected = prop (PropName "selected")

selectedIndex :: forall r i. Int -> IProp (selectedIndex :: Int | r) i
selectedIndex = prop (PropName "selectedIndex")

placeholder :: forall r i. String -> IProp (placeholder :: String | r) i
placeholder = prop (PropName "placeholder")

autocomplete :: forall r i. Boolean -> IProp (autocomplete :: I.OnOff | r) i
autocomplete = prop (PropName "autocomplete") <<< (\b -> if b then I.On else I.Off)

list :: forall r i. String -> IProp (list :: String | r) i
list = attr (AttrName "list")

autofocus :: forall r i. Boolean -> IProp (autofocus :: Boolean | r) i
autofocus = prop (PropName "autofocus")

multiple :: forall r i. Boolean -> IProp (multiple :: Boolean | r) i
multiple = prop (PropName "multiple")

accept :: forall r i. I.InputAcceptType -> IProp (accept :: I.InputAcceptType | r) i
accept = prop (PropName "accept")

pattern :: forall r i. String -> IProp (pattern :: String | r) i
pattern = prop (PropName "pattern")

autoplay :: forall r i. Boolean -> IProp (autoplay :: Boolean | r) i
autoplay = prop (PropName "autoplay")

controls :: forall r i. Boolean -> IProp (controls :: Boolean | r) i
controls = prop (PropName "controls")

loop :: forall r i. Boolean -> IProp (loop :: Boolean | r) i
loop = prop (PropName "loop")

muted :: forall r i. Boolean -> IProp (muted :: Boolean | r) i
muted = prop (PropName "muted")

poster :: forall r i. String -> IProp (poster :: String | r) i
poster = prop (PropName "poster")

preload :: forall r i. I.PreloadValue -> IProp (preload :: I.PreloadValue | r) i
preload = prop (PropName "preload")

draggable :: forall r i. Boolean -> IProp (draggable :: Boolean | r) i
draggable = prop (PropName "draggable")

tabIndex :: forall r i. Int -> IProp (tabIndex :: Int | r) i
tabIndex = prop (PropName "tabIndex")
