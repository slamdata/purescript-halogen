module Halogen.Component
  ( Component
  , ComponentSpec
  , mkComponent
  , unComponent
  , hoist
  , EvalSpec
  , mkEval
  , defaultEval
  , ComponentSlotBox
  , ComponentSlot(..)
  , componentSlot
  , ComponentSlotSpec
  , mkComponentSlot
  , unComponentSlot
  , hoistSlot
  ) where

import Prelude

import Data.Bifunctor (bimap, lmap)
import Data.Bifunctor.Wrap (Wrap(..))
import Data.Coyoneda (unCoyoneda)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (under)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple)
import Halogen.Data.Slot (Slot, SlotStorage)
import Halogen.Data.Slot as Slot
import Halogen.HTML.Core as HC
import Halogen.Query.HalogenM (HalogenM)
import Halogen.Query.HalogenM as HM
import Halogen.Query.HalogenQ (HalogenQ(..))
import Halogen.VDom.Thunk (Thunk)
import Halogen.VDom.Thunk as Thunk
import Prim.Row as Row
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (HTMLElement)

-- | The "public" type for a component, with details of the component internals
-- | existentially hidden.
-- |
-- |   `HTML`
-- | - `query` is the query algebra; the requests that can be made of the
-- |   component
-- | - `input` is the input value that will be received when the parent of
-- |   this component renders
-- | - `output` is the type of messages the component can raise
-- | - `m` is the effect monad used during evaluation
data Component
  (query :: Type -> Type)
  (input :: Type)
  (output :: Type)
  (m :: Type -> Type)

-- | The spec for a component.
-- |
-- | The type variables involved:
-- | - `state` is the component's state
-- | - `query` is the query algebra; the requests that can be made of the
-- |   component
-- | - `action` is the type of actions; messages internal to the component that
-- |   can be evaluated
-- | - `slots` is the set of slots for addressing child components
-- | - `input` is the input value that will be received when the parent of
-- |   this component renders
-- | - `output` is the type of messages the component can raise
-- | - `m` is the effect monad used during evaluation
-- |
-- | The values in the record:
-- | - `initialState` is a function that accepts an input value and produces the
-- |   state the component will start with. If the input value is unused
-- |   (`Unit`), or irrelevant to the state construction, this will often be
-- |   `const ?someInitialStateValue`.
-- | - `render` is a function that accepts the component's current state and
-- |   produces a value to render (`HTML` usually). The rendered output can
-- |   raise actions that will be handled in `eval`.
-- | - `eval` is a function that handles the `HalogenQ` algebra that deals with
-- |   component lifecycle, handling actions, and responding to requests.
type ComponentSpec state query action slots input output m =
  { initialState :: input -> state
  , render :: state -> HC.HTML (ComponentSlot slots m action) action
  , eval :: HalogenQ query action input ~> HalogenM state action slots output m
  }

-- | Constructs a [`Component`](#t:Component) from a [`ComponentSpec`](#t:ComponentSpec).
mkComponent
  :: forall state query action slots input output m
   . ComponentSpec state query action slots input output m
  -> Component query input output m
mkComponent = unsafeCoerce

-- | Exposes the inner details of a [`Component`](#t:Component) to a function
-- | to produce a new result.
-- |
-- | The hidden details will not be allowed to be revealed in the result
-- | of the function - if any of the hidden types (state, action, set of slots)
-- | appear in the result, the compiler will complain about an escaped skolem.
unComponent
  :: forall query input output m a
   . (forall state action slots. ComponentSpec state query action slots input output m -> a)
  -> Component query input output m
  -> a
unComponent = unsafeCoerce

-- | Changes the [`Component`](#t:Component)'s `m` type. A use case for this
-- | might be to interpret some `Free` monad as `Aff` so the component can be
-- | used with `runUI`.
hoist
  :: forall query input output m m'
   . Functor m'
  => (m ~> m')
  -> Component query input output m
  -> Component query input output m'
hoist nat = unComponent \c ->
  mkComponent
    { initialState: c.initialState
    , render: lmap (hoistSlot nat) <<< c.render
    , eval: HM.hoist nat <<< c.eval
    }

-- | The spec record that `mkEval` accepts to construct a component `eval`
-- | function.
-- |
-- | It's not a requirement to use `mkEval`, and sometimes it's preferrable
-- | to write a component `eval` function from scratch, but often `mkEval` is
-- | more convenient for common cases.
-- |
-- | See below for more details about `mkEval` and `defaultEval`.
type EvalSpec state query action slots input output m =
  { handleAction :: action -> HalogenM state action slots output m Unit
  , handleQuery :: forall a. query a -> HalogenM state action slots output m (Maybe a)
  , receive :: input -> Maybe action
  , initialize :: Maybe action
  , finalize :: Maybe action
  }

-- | A default value for `mkEval` that will result in an `eval` that nothing at
-- | all - all incoming actions and queries will be ignored, and no receiver,
-- | initializer, or finalizer will be specified.
-- |
-- | Usually this will be used with record update syntax to override fields to
-- | specify things as needed. If a component only needs to handle actions,
-- | for instance, a usage might be something like this:
-- |
-- | ```purescript
-- | H.mkComponent
-- |   { initialState
-- |   , render
-- |   , eval: H.mkEval (H.defaultEval { handleAction = ?handleAction })
-- |   }
-- | ```
defaultEval :: forall state query action slots input output m. EvalSpec state query action slots input output m
defaultEval =
  { handleAction: const (pure unit)
  , handleQuery: const (pure Nothing)
  , receive: const Nothing
  , initialize: Nothing
  , finalize: Nothing
  }

-- | Accepts an `EvalSpec` to produce an `eval` function for a component. For
-- | example:
-- |
-- | ```purescript
-- | -- use `defaultEval` and override fields selectively
-- | H.mkEval (H.defaultEval { handleAction = ?handleAction })
-- |
-- | -- or specify all the fields in the `EvalSpec`
-- | H.mkEval
-- |   { handleAction: ?handleAction
-- |   , handleQuery: ?handleQuery
-- |   , receive: ?receive
-- |   , initialize: ?initialize
-- |   , finalize: ?finalize
-- |   }
-- | ```
mkEval
  :: forall state query action slots input output m
   . EvalSpec state query action slots input output m
  -> HalogenQ query action input
  ~> HalogenM state action slots output m
mkEval args = case _ of
  Initialize a ->
    traverse_ args.handleAction args.initialize $> a
  Finalize a ->
    traverse_ args.handleAction args.finalize $> a
  Receive i a ->
    traverse_ args.handleAction (args.receive i) $> a
  Action action a ->
    args.handleAction action $> a
  Query req f ->
    unCoyoneda (\g → map (maybe (f unit) g) <<< args.handleQuery) req

-- | A slot for a child component in a component's rendered content.
data ComponentSlotBox
  (slots :: Row Type)
  (m :: Type -> Type)
  (action :: Type)

instance functorComponentSlotBox :: Functor (ComponentSlotBox slots m) where
  map f = unComponentSlot \slot ->
    mkComponentSlot $ slot { output = map f <$> slot.output }

data ComponentSlot slots m action
  = ComponentSlot (ComponentSlotBox slots m action)
  | ThunkSlot (Thunk (HC.HTML (ComponentSlot slots m action)) action)
  | PortalSlot HTMLElement (HC.HTML (ComponentSlot slots m action) action)

instance functorComponentSlot :: Functor (ComponentSlot slots m) where
  map f = case _ of
    ComponentSlot box -> ComponentSlot (map f box)
    ThunkSlot thunk -> ThunkSlot (Thunk.mapThunk (under Wrap (map f) <<< lmap (map f)) thunk)
    PortalSlot elem inner -> PortalSlot elem (bimap (map f) f inner)

-- | Constructs a [`ComponentSlot`](#t:ComponentSlot).
-- |
-- | Takes:
-- | - the slot address label
-- | - the slot address index
-- | - the component for the slot
-- | - the input value to pass to the component
-- | - a function mapping outputs from the component to a query in the parent
componentSlot
  :: forall query input output slots m action label slot _1
   . Row.Cons label (Slot query output slot) _1 slots
  => IsSymbol label
  => Ord slot
  => Proxy label
  -> slot
  -> Component query input output m
  -> input
  -> (output -> Maybe action)
  -> ComponentSlotBox slots m action
componentSlot label p comp input output =
  mkComponentSlot
    { get: Slot.lookup label p
    , pop: Slot.pop label p
    , set: Slot.insert label p
    , component: comp
    , input: input
    , output
    }

-- | The internal representation used for a [`ComponentSlot`](#t:ComponentSlot).
type ComponentSlotSpec query input output slots m action =
  { get :: forall slot. SlotStorage slots slot -> Maybe (slot query output)
  , pop :: forall slot. SlotStorage slots slot -> Maybe (Tuple (slot query output) (SlotStorage slots slot))
  , set :: forall slot. slot query output -> SlotStorage slots slot -> SlotStorage slots slot
  , component :: Component query input output m
  , input :: input
  , output :: output -> Maybe action
  }

-- | Constructs [`ComponentSlotBox`](#t:ComponentSlot) from a [`ComponentSlotSpec`](#t:ComponentSlotSpec).
mkComponentSlot
  :: forall query input output slots m action
   . ComponentSlotSpec query input output slots m action
  -> ComponentSlotBox slots m action
mkComponentSlot = unsafeCoerce

-- | Exposes the inner details of a [`ComponentSlot`](#t:ComponentSlot) to a
-- | function to produce a new result.
-- |
-- |  The hidden details will not be allowed to be revealed in the result
-- | of the function - if any of the hidden types (state, action, set of slots)
-- | appear in the result, the compiler will complain about an escaped skolem.
unComponentSlot
  :: forall slots m action a
   . (forall query input output. ComponentSlotSpec query input output slots m action -> a)
  -> ComponentSlotBox slots m action
  -> a
unComponentSlot = unsafeCoerce

-- | Changes the [`ComponentSlot`](#t:ComponentSlot)'s `m` type.
hoistSlot
  :: forall slots m m' action
   . Functor m'
  => (m ~> m')
  -> ComponentSlot slots m action
  -> ComponentSlot slots m' action
hoistSlot nat = case _ of
  ComponentSlot cs ->
    cs # unComponentSlot \slot ->
      ComponentSlot $ mkComponentSlot $ slot { component = hoist nat slot.component }
  ThunkSlot t ->
    ThunkSlot $ Thunk.hoist (lmap (hoistSlot nat)) t
  PortalSlot el inner ->
    PortalSlot el (lmap (hoistSlot nat) inner)
