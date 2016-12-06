module Halogen.Aff.Driver
  ( RenderSpec
  , runUI
  , module Halogen
  , module Halogen.Effects
  , module Exports
  ) where

import Prelude

import Control.Coroutine as CR
import Control.Monad.Aff (Aff, forkAff, forkAll, runAff)
import Control.Monad.Aff.AVar as AV
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error, throwException)
import Control.Monad.Eff.Ref (Ref, modifyRef, writeRef, readRef, newRef)
import Control.Parallel (parSequence_)

import Data.Lazy (force)
import Data.List ((:))
import Data.List as L
import Data.Either (Either(..))
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (for_, traverse_, sequence_)
import Data.Tuple (Tuple(..))

import Halogen (HalogenIO)
import Halogen.Aff.Driver.Eval (LifecycleHandlers, eval, handleLifecycle)
import Halogen.Aff.Driver.State (ComponentType(..), DriverState(..), DriverStateX, initDriverState, unDriverStateX, mkDriverStateXRef)
import Halogen.Component (Component, ComponentSlot, unComponent, unComponentSlot)
import Halogen.Data.OrdBox (OrdBox)
import Halogen.Effects (HalogenEffects)

import Halogen.Aff.Driver.State (ComponentType(..)) as Exports

type RenderSpec h r eff =
  { render
      :: forall s f g p o
       . (forall x. f x -> Eff (HalogenEffects eff) Unit)
      -> (ComponentSlot h g (Aff (HalogenEffects eff)) p (f Unit) -> Eff (HalogenEffects eff) (Ref (DriverStateX h r g eff)))
      -> h (ComponentSlot h g (Aff (HalogenEffects eff)) p (f Unit)) (f Unit)
      -> ComponentType
      -> Ref (DriverState h r s f g p o eff)
      -> Maybe (r s f g p o eff)
      -> Eff (HalogenEffects eff) (r s f g p o eff)
  , renderChild
      :: forall s f g p o
       . Int
      -> Ref (DriverState h r s f g p o eff)
      -> Maybe (r s f g p o eff)
      -> Eff (HalogenEffects eff) (r s f g p o eff)
  }

runUI
  :: forall h r f o eff
   . RenderSpec h r eff
  -> Component h f o (Aff (HalogenEffects eff))
  -> Aff (HalogenEffects eff) (HalogenIO f o (Aff (HalogenEffects eff)))
runUI renderSpec component = do
  fresh <- liftEff $ newRef 0
  handleLifecycle \lchs -> liftEff do
    listeners <- newRef M.empty
    runComponent (rootHandler listeners) fresh lchs Root component
      >>= readRef
      >>= unDriverStateX \st ->
        pure
          { query: evalF st.selfRef
          , subscribe: subscribe fresh listeners
          }

  where

  evalF
    :: forall s f' g p o'
     . Ref (DriverState h r s f' g p o' eff)
    -> f'
    ~> Aff (HalogenEffects eff)
  evalF ref = eval render ref

  rootHandler
    :: Ref (M.Map Int (AV.AVar o))
    -> o
    -> Aff (HalogenEffects eff) Unit
  rootHandler ref message = do
    listeners <- liftEff $ readRef ref
    void $ forkAll $ map (\var -> AV.putVar var message) listeners

  subscribe
    :: Ref Int
    -> Ref (M.Map Int (AV.AVar o))
    -> CR.Consumer o (Aff (HalogenEffects eff)) Unit
    -> Aff (HalogenEffects eff) Unit
  subscribe fresh ref consumer = do
    inputVar <- AV.makeVar
    listenerId <- liftEff do
      listenerId <- readRef fresh
      modifyRef fresh (_ + 1)
      modifyRef ref (M.insert listenerId inputVar)
      pure listenerId
    let producer = CR.producer (Left <$> AV.takeVar inputVar)
    void $ forkAff do
      CR.runProcess (CR.connect producer consumer)
      liftEff $ modifyRef ref (M.delete listenerId)
      AV.killVar inputVar (error "ended")

  runComponent
    :: forall f' o'
     . (o' -> Aff (HalogenEffects eff) Unit)
    -> Ref Int
    -> Ref (LifecycleHandlers eff)
    -> ComponentType
    -> Component h f' o' (Aff (HalogenEffects eff))
    -> Eff (HalogenEffects eff) (Ref (DriverStateX h r f' eff))
  runComponent handler fresh lchs componentType = unComponent \c -> do
    keyId <- readRef fresh
    modifyRef fresh (_ + 1)
    var <- initDriverState c componentType handler keyId fresh
    unDriverStateX (render lchs <<< _.selfRef) =<< readRef var
    squashChildInitializers lchs =<< readRef var
    pure var

  render
    :: forall s f' g p o'
     . Ref (LifecycleHandlers eff)
    -> Ref (DriverState h r s f' g p o' eff)
    -> Eff (HalogenEffects eff) Unit
  render lchs var = readRef var >>= \(DriverState ds) -> do
    childrenVar <- newRef M.empty
    oldChildren <- newRef ds.children
    let
      selfEval = evalF ds.selfRef
      handler :: forall x. f' x -> Aff (HalogenEffects eff) Unit
      handler = void <<< selfEval
      handler' :: forall x. f' x -> Aff (HalogenEffects eff) Unit
      handler' = maybe handler (\_ -> queuingHandler ds.selfRef handler) ds.pendingIn
    rendering <-
      renderSpec.render
        (handleAff <<< selfEval)
        (renderChild handler' ds.fresh ds.mkOrdBox oldChildren childrenVar lchs)
        (ds.component.render ds.state)
        ds.componentType
        ds.selfRef
        ds.rendering
    children <- readRef childrenVar
    traverse_ (addFinalizer lchs <=< readRef) =<< readRef oldChildren
    writeRef var $
      DriverState
        { rendering: Just rendering
        , componentType: ds.componentType
        , component: ds.component
        , state: ds.state
        , children
        , mkOrdBox: ds.mkOrdBox
        , selfRef: ds.selfRef
        , handler: ds.handler
        , pendingIn: ds.pendingIn
        , pendingOut: ds.pendingOut
        , keyId: ds.keyId
        , fresh: ds.fresh
        }

  queuingHandler
    :: forall s f' g p o' x
     . Ref (DriverState h r s f' g p o' eff)
    -> (f' x -> Aff (HalogenEffects eff) Unit)
    -> f' x
    -> Aff (HalogenEffects eff) Unit
  queuingHandler var handler message = do
    DriverState (ds@{ pendingIn }) <- liftEff (readRef var)
    case pendingIn of
      Nothing -> do
        liftEff $ writeRef var (DriverState ds)
        handler message
      Just p ->
        liftEff $ writeRef var (DriverState ds { pendingIn = Just (handler message : p) })

  renderChild
    :: forall f' g p
     . (forall x. f' x -> Aff (HalogenEffects eff) Unit)
    -> Ref Int
    -> (p -> OrdBox p)
    -> Ref (M.Map (OrdBox p) (Ref (DriverStateX h r g eff)))
    -> Ref (M.Map (OrdBox p) (Ref (DriverStateX h r g eff)))
    -> Ref (LifecycleHandlers eff)
    -> ComponentSlot h g (Aff (HalogenEffects eff)) p (f' Unit)
    -> Eff (HalogenEffects eff) (Ref (DriverStateX h r g eff))
  renderChild handler fresh mkOrdBox childrenInRef childrenOutRef lchs =
    unComponentSlot \p ctor k -> do
      childrenIn <- readRef childrenInRef
      var <- case M.pop (mkOrdBox p) childrenIn of
        Just (Tuple existing childrenIn') -> do
          writeRef childrenInRef childrenIn'
          pure existing
        Nothing ->
          runComponent (maybe (pure unit) handler <<< k) fresh lchs Child (force ctor)
      modifyRef childrenOutRef (M.insert (mkOrdBox p) var)
      readRef var >>= unDriverStateX \st -> do
        r <- renderSpec.renderChild st.keyId st.selfRef st.rendering
        writeRef st.selfRef $ DriverState $ st { rendering = Just r }
        pure $ mkDriverStateXRef st.selfRef

  squashChildInitializers
    :: forall f'
     . Ref (LifecycleHandlers eff)
    -> DriverStateX h r f' eff
    -> Eff (HalogenEffects eff) Unit
  squashChildInitializers ref =
    unDriverStateX \st -> do
      let parentInitializer = evalF st.selfRef <$> st.component.initializer
      modifyRef ref \lchs ->
        { initializers: pure $ do
            parSequence_ (L.reverse lchs.initializers)
            sequence_ parentInitializer
            handlePending st.selfRef
        , finalizers: lchs.finalizers
        }

  handlePending
    :: forall s f' g p o'
     . Ref (DriverState h r s f' g p o' eff)
    -> Aff (HalogenEffects eff) Unit
  handlePending ref = do
    DriverState (dsi@{ pendingIn }) <- liftEff (readRef ref)
    liftEff $ writeRef ref (DriverState dsi { pendingIn = Nothing })
    for_ pendingIn (forkAll <<< L.reverse)
    DriverState (dso@{ pendingOut, handler }) <- liftEff (readRef ref)
    liftEff $ writeRef ref (DriverState dso { pendingOut = Nothing })
    for_ pendingOut (forkAll <<< map handler <<< L.reverse)

  addFinalizer
    :: forall f'
     . Ref (LifecycleHandlers eff)
    -> DriverStateX h r f' eff
    -> Eff (HalogenEffects eff) Unit
  addFinalizer ref =
    unDriverStateX \st -> do
      for_ (evalF st.selfRef <$> st.component.finalizer) \f ->
        modifyRef ref (\lchs ->
          { initializers: lchs.initializers
          , finalizers: f : lchs.finalizers
          })
      for_ st.children (addFinalizer ref <=< readRef)

-- | TODO: we could do something more intelligent now this isn't baked into the
-- | virtual-dom rendering. Perhaps write to an avar when an error occurs...
-- | something other than a runtime exception anyway.
handleAff
  :: forall eff a
   . Aff (HalogenEffects eff) a
  -> Eff (HalogenEffects eff) Unit
handleAff = void <<< runAff throwException (const (pure unit))

peekVar :: forall eff a. AV.AVar a -> Aff (avar :: AV.AVAR | eff) a
peekVar v = do
  a <- AV.takeVar v
  AV.putVar v a
  pure a
