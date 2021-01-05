module Halogen.VDom.Driver
  ( runUI
  , module Halogen.Aff.Driver
  ) where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried as EFn
import Halogen.Aff.Driver (HalogenIO)
import Halogen.Aff.Driver as AD
import Halogen.Aff.Driver.State (RenderStateX, unRenderStateX)
import Halogen.Component (Component, ComponentSlot(..), ComponentSlotBox)
import Halogen.HTML.Core (HTML(..), Prop)
import Halogen.Query.Input (Input)
import Halogen.VDom as V
import Halogen.VDom.DOM.Prop as VP
import Halogen.VDom.Thunk (Thunk)
import Halogen.VDom.Thunk as Thunk
import Halogen.VDom.Util as Util
import Unsafe.Reference (unsafeRefEq)
import Web.DOM.Document (Document) as DOM
import Web.DOM.Element (Element) as DOM
import Web.DOM.Node (Node, appendChild, removeChild, parentNode, nextSibling, insertBefore) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement (HTMLElement) as DOM
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document) as DOM

type VHTML action slots =
  V.VDom (Array (Prop (Input action))) (ComponentSlot slots Aff action)

type ChildRenderer action slots
  = ComponentSlotBox slots Aff action -> Effect (RenderStateX RenderState)

newtype RenderState state action slots output =
  RenderState
    { node :: DOM.Node
    , machine :: V.Step (VHTML action slots) DOM.Node
    , renderChildRef :: Ref (ChildRenderer action slots)
    }

type HTMLThunk slots action =
  Thunk (HTML (ComponentSlot slots Aff action)) action

type WidgetState slots action =
  Maybe (V.Step (HTMLThunk slots action) DOM.Node)

mkSpec
  :: forall action slots
   . (Input action -> Effect Unit)
  -> Ref (ChildRenderer action slots)
  -> DOM.Document
  -> V.VDomSpec
      (Array (VP.Prop (Input action)))
      (ComponentSlot slots Aff action)
mkSpec handler renderChildRef document =
  V.VDomSpec { buildWidget, buildAttributes, document }
  where
  buildAttributes
    :: DOM.Element
    -> V.Machine (Array (VP.Prop (Input action))) Unit
  buildAttributes = VP.buildProp handler

  buildWidget
    :: V.VDomSpec (Array (VP.Prop (Input action))) (ComponentSlot slots Aff action)
    -> V.Machine (ComponentSlot slots Aff action) DOM.Node
  buildWidget spec = render
    where
    render :: V.Machine (ComponentSlot slots Aff action) DOM.Node
    render = EFn.mkEffectFn1 \slot ->
      case slot of
        ComponentSlot cs ->
          EFn.runEffectFn1 renderComponentSlot cs
        ThunkSlot t -> do
          step <- EFn.runEffectFn1 buildThunk t
          pure $ V.mkStep $ V.Step (V.extract step) (Just step) patch done
        PortalSlot elem inner ->
          EFn.runEffectFn2 renderPortalSlot elem inner

    patch
      :: EFn.EffectFn2
            (WidgetState slots action)
            (ComponentSlot slots Aff action)
            (V.Step (ComponentSlot slots Aff action) DOM.Node)
    patch = EFn.mkEffectFn2 \st slot ->
      case st of
        Just step -> case slot of
          ComponentSlot cs -> do
            EFn.runEffectFn1 V.halt step
            EFn.runEffectFn1 renderComponentSlot cs
          ThunkSlot t -> do
            step' <- EFn.runEffectFn2 V.step step t
            pure $ V.mkStep $ V.Step (V.extract step') (Just step') patch done
          PortalSlot elem inner -> do
            EFn.runEffectFn1 V.halt step
            EFn.runEffectFn2 renderPortalSlot elem inner
        _ ->
          EFn.runEffectFn1 render slot

    buildThunk :: V.Machine (HTMLThunk slots action) DOM.Node
    buildThunk = Thunk.buildThunk unwrap spec

    renderComponentSlot
      :: EFn.EffectFn1
            (ComponentSlotBox slots Aff action)
            (V.Step (ComponentSlot slots Aff action) DOM.Node)
    renderComponentSlot = EFn.mkEffectFn1 \cs -> do
      renderChild <- Ref.read renderChildRef
      rsx <- renderChild cs
      let node = getNode rsx
      pure $ V.mkStep $ V.Step node Nothing patch done

    renderPortalSlot
      :: EFn.EffectFn2
            DOM.HTMLElement
            (HTML (ComponentSlot slots Aff action) action)
            (V.Step (ComponentSlot slots Aff action) DOM.Node)
    renderPortalSlot = EFn.mkEffectFn2 \elem inner -> do
      step <- EFn.runEffectFn1 buildThunk (Thunk.thunked (\_ _ -> false) (const inner) unit)
      emptyNode <- EFn.runEffectFn2 Util.createTextNode "" document
      void $ DOM.appendChild (V.extract step) (HTMLElement.toNode elem)
      pure $ V.mkStep $ V.Step emptyNode (Just step) patch done

  done :: EFn.EffectFn1 (WidgetState slots action) Unit
  done = EFn.mkEffectFn1 case _ of
    Just step -> EFn.runEffectFn1 V.halt step
    _ -> pure unit

  getNode :: RenderStateX RenderState -> DOM.Node
  getNode = unRenderStateX (\(RenderState { node }) -> node)

runUI
  :: forall query input output
   . Component query input output Aff
  -> input
  -> DOM.HTMLElement
  -> Aff (HalogenIO query output Aff)
runUI component i element = do
  document <- liftEffect $ HTMLDocument.toDocument <$> (DOM.document =<< DOM.window)
  AD.runUI (renderSpec document element) component i

renderSpec
  :: DOM.Document
  -> DOM.HTMLElement
  -> AD.RenderSpec RenderState
renderSpec document container =
    { render
    , renderChild: identity
    , removeChild
    , dispose: removeChild
    }
  where
  render
    :: forall state action slots output
     . (Input action -> Effect Unit)
    -> (ComponentSlotBox slots Aff action -> Effect (RenderStateX RenderState))
    -> HTML (ComponentSlot slots Aff action) action
    -> Maybe (RenderState state action slots output)
    -> Effect (RenderState state action slots output)
  render handler child (HTML vdom) =
    case _ of
      Nothing -> do
        renderChildRef <- Ref.new child
        let spec = mkSpec handler renderChildRef document
        machine <- EFn.runEffectFn1 (V.buildVDom spec) vdom
        let node = V.extract machine
        void $ DOM.appendChild node (HTMLElement.toNode container)
        pure $ RenderState { machine, node, renderChildRef }
      Just (RenderState { machine, node, renderChildRef }) -> do
        Ref.write child renderChildRef
        parent <- DOM.parentNode node
        nextSib <- DOM.nextSibling node
        machine' <- EFn.runEffectFn2 V.step machine vdom
        let newNode = V.extract machine'
        when (not unsafeRefEq node newNode) do
          substInParent newNode nextSib parent
        pure $ RenderState { machine: machine', node: newNode, renderChildRef }

removeChild :: forall state action slots output. RenderState state action slots output -> Effect Unit
removeChild (RenderState { node }) = do
  npn <- DOM.parentNode node
  traverse_ (\pn -> DOM.removeChild node pn) npn

substInParent :: DOM.Node -> Maybe DOM.Node -> Maybe DOM.Node -> Effect Unit
substInParent newNode (Just sib) (Just pn) = void $ DOM.insertBefore newNode sib pn
substInParent newNode Nothing (Just pn) = void $ DOM.appendChild newNode pn
substInParent _ _ _ = pure unit
