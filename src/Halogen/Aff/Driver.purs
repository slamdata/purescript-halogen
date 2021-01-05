module Halogen.Aff.Driver
  ( RenderSpec
  , runUI
  , module Halogen
  ) where

import Prelude

import Control.Monad.Fork.Class (fork)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Parallel (parSequence_)
import Data.List ((:))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), maybe, isJust, isNothing)
import Data.Traversable (for_, sequence_, traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, killFiber)
import Effect.Class (liftEffect)
import Effect.Console (warn)
import Effect.Exception (error, throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import FRP.Event as Event
import Halogen (HalogenIO)
import Halogen.Aff.Driver.Eval as Eval
import Halogen.Aff.Driver.State (DriverState(..), DriverStateRef(..), DriverStateX, LifecycleHandlers, RenderStateX, initDriverState, mapDriverState, renderStateX, renderStateX_, unDriverStateX)
import Halogen.Component (Component, ComponentSlot, ComponentSlotBox, unComponent, unComponentSlot)
import Halogen.Data.Slot as Slot
import Halogen.HTML.Core as HC
import Halogen.Query.HalogenQ as HQ
import Halogen.Query.Input (Input)
import Halogen.Query.Input as Input

-- | `RenderSpec` allows for alternative driver implementations without the need
-- | to provide all of the driver machinery again, focusing just on the code
-- | needed to render components.
-- |
-- | `r` is the type for the "render state" for the driver. This is a value
-- | that is stored for each component, that allows the driver to persist
-- | state between each rendering of a component. This will differ entirely
-- | for each driver. `r` accepts a number of parameters that will be
-- | explained below.
-- |
-- | The "inner" type variables, used by `r` and the other functions are:
-- | - `s` is the state type for the component.
-- | - `act` is the action type for the component
-- | - `ps` is the set of slots for addressing child components
-- | - `o` is the output message type for the component
-- |
-- | Note that none of these variables can escape `RenderSpec`'s functions. They
-- | need to be instantiated with each function call, as the same `RenderSpec`
-- | is used to deal with all components in the hierarchy.
-- |
-- | The `render` function is the main part of the spec, it accepts:
-- | - A "handler" function, for evaluating component queries. This is used to
-- |   implement event handlers in HTML-based drivers.
-- | - A "child" function for dealing with the rendering of children, returning
-- |   the render state for the child component in an existentially hidden
-- |   package. This return value would commonly be used to extract the rendered
-- |   subtree for the child to graft it in place of the child slot. The
-- |   existential package can be unwrapped with `Halogen.Aff.Driver.State.unRenderStateX`.
-- | - The HTML value to render, parameterised by the slot type for the
-- |   component's children. This slot type is what the "child" function
-- |   accepts.
-- | - The previous render state for the component. If the component has not
-- |   previously been initalized, this will be `Nothing`.
-- |
-- | The render function then returns the updated (or initial) render state for
-- | the component, which will be fed back into `render` the next time the
-- | component needs to update.
-- |
-- | The `renderChild` function's behaviour will be highly dependent on the
-- | particular driver implementing `RenderSpec`. Its purpose is to take a
-- | driver render state for a component and produce a new one that may remap
-- | the rendered value to be something more suitable for grafting during
-- | `render` of the parent. For the built-in `halogen-vdom` driver this is
-- | just `identity`. For the `virtual-dom` driver it wraps the rendered HTML
-- | in a widget, to prevent the `virtual-dom` algorithm from re-diffing
-- | values that we know are unchanged.
-- |
-- | The `removeChild` function is for drivers that need to perform some special
-- | cleanup when removing a component from the hierarchy. In the `halogen-vdom`
-- | driver this actually performs the `removeChild` from the DOM. For the
-- | `virtual-dom` driver nothing needs to happen here, so it is
-- | `const (pure unit)`.
-- |
-- | The `dispose` function is called when the top level component is disposed of
-- | via `HalogenIO`.
type RenderSpec r =
  { render
      :: forall s act ps o
       . (Input act -> Effect Unit)
      -> (ComponentSlotBox ps Aff act -> Effect (RenderStateX r))
      -> HC.HTML (ComponentSlot ps Aff act) act
      -> Maybe (r s act ps o)
      -> Effect (r s act ps o)
  , renderChild :: forall s act ps o. r s act ps o -> r s act ps o
  , removeChild :: forall s act ps o. r s act ps o -> Effect Unit
  , dispose :: forall s act ps o. r s act ps o -> Effect Unit
  }

runUI
  :: forall r f i o
   . RenderSpec r
  -> Component f i o Aff
  -> i
  -> Aff (HalogenIO f o Aff)
runUI renderSpec component i = do
  lchs <- liftEffect newLifecycleHandlers
  fresh <- liftEffect $ Ref.new 0
  disposed <- liftEffect $ Ref.new false
  Eval.handleLifecycle lchs do
    eio <- Event.create
    dsx <- Ref.read =<< runComponent lchs (liftEffect <<< eio.push) i component
    unDriverStateX (\st ->
      pure
        { query: evalDriver disposed st.selfRef
        , messages: eio.event
        , dispose: dispose disposed lchs dsx
        }) dsx

  where

  evalDriver
    :: forall s f' act ps i' o'
     . Ref Boolean
    -> Ref (DriverState r s f' act ps i' o')
    -> forall a. (f' a -> Aff (Maybe a))
  evalDriver disposed ref q =
    liftEffect (Ref.read disposed) >>=
      if _
        then pure Nothing
        else Eval.evalQ render ref q

  runComponent
    :: forall f' i' o'
     . Ref LifecycleHandlers
    -> (o' -> Aff Unit)
    -> i'
    -> Component f' i' o' Aff
    -> Effect (Ref (DriverStateX r f' o'))
  runComponent lchs handler j = unComponent \c -> do
    lchs' <- newLifecycleHandlers
    var <- initDriverState c j handler lchs'
    pre <- Ref.read lchs
    Ref.write { initializers: L.Nil, finalizers: pre.finalizers } lchs
    unDriverStateX (render lchs <<< _.selfRef) =<< Ref.read var
    squashChildInitializers lchs pre.initializers =<< Ref.read var
    pure var

  render
    :: forall s f' act ps i' o'
     . Ref LifecycleHandlers
    -> Ref (DriverState r s f' act ps i' o')
    -> Effect Unit
  render lchs var = Ref.read var >>= \(DriverState ds) -> do
    shouldProcessHandlers <- isNothing <$> Ref.read ds.pendingHandlers
    when shouldProcessHandlers $ Ref.write (Just L.Nil) ds.pendingHandlers
    Ref.write Slot.empty ds.childrenOut
    Ref.write ds.children ds.childrenIn
    let
      -- The following 3 defs are working around a capture bug, see #586
      pendingHandlers = identity ds.pendingHandlers
      pendingQueries = identity ds.pendingQueries
      selfRef = identity ds.selfRef
      handler :: Input act -> Aff Unit
      handler = Eval.queueOrRun pendingHandlers <<< void <<< Eval.evalF render selfRef
      childHandler :: act -> Aff Unit
      childHandler = Eval.queueOrRun pendingQueries <<< handler <<< Input.Action
    rendering <-
      renderSpec.render
        (Eval.handleAff <<< handler)
        (renderChild lchs childHandler ds.childrenIn ds.childrenOut)
        (ds.component.render ds.state)
        ds.rendering
    children <- Ref.read ds.childrenOut
    childrenIn <- Ref.read ds.childrenIn
    Slot.foreachSlot childrenIn \(DriverStateRef childVar) -> do
      childDS <- Ref.read childVar
      renderStateX_ renderSpec.removeChild childDS
      finalize lchs childDS
    flip Ref.modify_ ds.selfRef $ mapDriverState \ds' ->
      ds' { rendering = Just rendering, children = children }
    when shouldProcessHandlers do
      flip tailRecM unit \_ -> do
        handlers <- Ref.read pendingHandlers
        Ref.write (Just L.Nil) pendingHandlers
        traverse_ (Eval.handleAff <<< traverse_ fork <<< L.reverse) handlers
        mmore <- Ref.read pendingHandlers
        if maybe false L.null mmore
          then Ref.write Nothing pendingHandlers $> Done unit
          else pure $ Loop unit

  renderChild
    :: forall ps act
     . Ref LifecycleHandlers
    -> (act -> Aff Unit)
    -> Ref (Slot.SlotStorage ps (DriverStateRef r))
    -> Ref (Slot.SlotStorage ps (DriverStateRef r))
    -> ComponentSlotBox ps Aff act
    -> Effect (RenderStateX r)
  renderChild lchs handler childrenInRef childrenOutRef =
    unComponentSlot \slot -> do
      childrenIn <- slot.pop <$> Ref.read childrenInRef
      var <- case childrenIn of
        Just (Tuple (DriverStateRef existing) childrenIn') -> do
          Ref.write childrenIn' childrenInRef
          dsx <- Ref.read existing
          unDriverStateX (\st -> do
            flip Ref.write st.handlerRef $ maybe (pure unit) handler <<< slot.output
            Eval.handleAff $ Eval.evalM render st.selfRef (st.component.eval (HQ.Receive slot.input unit))) dsx
          pure existing
        Nothing ->
          runComponent lchs (maybe (pure unit) handler <<< slot.output) slot.input slot.component
      isDuplicate <- isJust <<< slot.get <$> Ref.read childrenOutRef
      when isDuplicate
        $ warn "Halogen: Duplicate slot address was detected during rendering, unexpected results may occur"
      Ref.modify_ (slot.set $ DriverStateRef var) childrenOutRef
      Ref.read var >>= renderStateX case _ of
        Nothing -> throw "Halogen internal error: child was not initialized in renderChild"
        Just r -> pure (renderSpec.renderChild r)

  squashChildInitializers
    :: forall f' o'
     . Ref LifecycleHandlers
    -> L.List (Aff Unit)
    -> DriverStateX r f' o'
    -> Effect Unit
  squashChildInitializers lchs preInits =
    unDriverStateX \st -> do
      let parentInitializer = Eval.evalM render st.selfRef (st.component.eval (HQ.Initialize unit))
      Ref.modify_ (\handlers ->
        { initializers: (do
            parSequence_ (L.reverse handlers.initializers)
            parentInitializer
            liftEffect do
              handlePending st.pendingQueries
              handlePending st.pendingOuts) : preInits
        , finalizers: handlers.finalizers
        }) lchs

  finalize
    :: forall f' o'
     . Ref LifecycleHandlers
    -> DriverStateX r f' o'
    -> Effect Unit
  finalize lchs = do
    unDriverStateX \st -> do
      cleanupSubscriptionsAndForks (DriverState st)
      let f = Eval.evalM render st.selfRef (st.component.eval (HQ.Finalize unit))
      Ref.modify_ (\handlers ->
        { initializers: handlers.initializers
        , finalizers: f : handlers.finalizers
        }) lchs
      Slot.foreachSlot st.children \(DriverStateRef ref) -> do
        dsx <- Ref.read ref
        finalize lchs dsx

  dispose :: forall f' o'
     . Ref Boolean
    -> Ref LifecycleHandlers
    -> DriverStateX r f' o'
    -> Aff Unit
  dispose disposed lchs dsx = Eval.handleLifecycle lchs do
    Ref.read disposed >>= if _ then
      pure unit
    else do
      Ref.write true disposed
      finalize lchs dsx
      dsx # unDriverStateX \{ selfRef } -> do
        (DriverState ds) <- liftEffect $ Ref.read selfRef
        for_ ds.rendering renderSpec.dispose

newLifecycleHandlers :: Effect (Ref LifecycleHandlers)
newLifecycleHandlers = Ref.new { initializers: L.Nil, finalizers: L.Nil }

handlePending :: Ref (Maybe (L.List (Aff Unit))) -> Effect Unit
handlePending ref = do
  queue <- Ref.read ref
  Ref.write Nothing ref
  for_ queue (Eval.handleAff <<< traverse_ fork <<< L.reverse)

cleanupSubscriptionsAndForks
  :: forall r s f act ps i o
   . DriverState r s f act ps i o
  -> Effect Unit
cleanupSubscriptionsAndForks (DriverState ds) = do
  traverse_ sequence_ =<< Ref.read ds.subscriptions
  Ref.write Nothing ds.subscriptions
  traverse_ (Eval.handleAff <<< killFiber (error "finalized")) =<< Ref.read ds.forks
  Ref.write M.empty ds.forks
