module Halogen.Component
  ( Component()
  , natG
  , ComponentF()
  , ComponentFC()
  , Render()
  , RenderF()
  , RenderFC()
  , Eval()
  , renderComponent
  , queryComponent
  , component
  , componentF
  , componentFC
  , ComponentState()
  , InstalledState()
  , ChildF()
  , installL
  , installR
  , installAll
  , QueryF()
  , query
  , liftQuery
  ) where

import Prelude

import Control.Monad.Aff (Aff())
import Control.Monad.Free (Free(), FreeC(), bindF, bindFC, liftF, mapF)
import Control.Monad.State (State(), runState)
import Control.Monad.State.Trans (StateT())
import Control.Monad.Trans (MonadTrans, lift)
import Control.Plus (Plus, empty)
import qualified Control.Monad.State.Class as CMS

import Data.Bifunctor (lmap)
import Data.Coyoneda (Natural(), Coyoneda())
import Data.Either (Either(..), either)
import Data.Functor.Coproduct (Coproduct(), coproduct, left, right)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Void (Void())
import qualified Data.Maybe.Unsafe as U

import Halogen.HTML.Core (HTML(..), install)
import Halogen.Query.StateF (StateF(..), get)

import qualified Data.Map as M

-- | Data type for Halogen components.
-- | - `s` - the type of the component state
-- | - `f` - the component's query algebra
-- | - `g` - the monad handling the component's non-state effects
-- | - `p` - the type of placeholders within the component, used to specify
-- |         "holes" in which child components can be installed.
newtype Component s f g p = Component
  { render :: State s (HTML p (f Unit))
  , query  :: Natural f (Free (Coproduct (StateF s) g))
  }

instance functorComponent :: Functor (Component s f g) where
  map f (Component c) = Component { render: lmap f <$> c.render, query: c.query }

-- TODO: is this useful, and what's a better name for it?
natG :: forall s f g g' p. (Functor g, Functor g') => Natural g g' -> Component s f g p -> Component s f g' p
natG f (Component c) =
  Component { render: c.render
            , query: \x -> mapF (coproduct left (right <<< f)) (c.query x)
            }

-- | A type alias for a Halogen component using `Free` for its query algebra.
type ComponentF s f = Component s (Free f)

-- | A type alias for a Halogen component using `FreeC` for their query algebra.
-- | This removes the need to write an explicit `Functor` instance for `f`.
type ComponentFC s f = ComponentF s (Coyoneda f)

-- | A type alias for a component `render` function.
type Render s p f = s -> HTML p (f Unit)

-- | A type alias for a component `render` function where the component is using
-- | `Free` for its query algebra.
type RenderF s p f = Render s p (Free f)

-- | A type alias for a component `render` function where the component is using
-- | `FreeC` for its query algebra.
type RenderFC s p f = RenderF s p (Coyoneda f)

-- | A type alias for a component `query` function that a value from the
-- | component's query algebra and returns a `Free` monad with state and `g`
-- | effects.
type Eval f s g = Natural f (Free (Coproduct (StateF s) g))

-- | Runs a component's `render` function with the specified state, returning
-- | the generated `HTML` and new state.
renderComponent :: forall s f g p. Component s f g p -> s -> Tuple (HTML p (f Unit)) s
renderComponent (Component c) = runState c.render

-- | Runs a compnent's `query` function with the specified query input and
-- | returns the pending computation as a `Free` monad.
queryComponent :: forall s f g p. Component s f g p -> Eval f s g
queryComponent (Component c) q = c.query q

-- | Builds a new [`Component`](#component) from a [`Render`](#render) and
-- | [`Eval`](#eval) function.
component :: forall s f g p. Render s p f -> Eval f s g -> Component s f g p
component r q = Component { render: CMS.gets r, query: q }

-- | Builds a new [`ComponentF`](#componentf) from a [`RenderF`](#renderf) and
-- | [`Eval`](#eval) function.
componentF :: forall s f g p. (Functor f, Functor g) => RenderF s p f -> Eval f s g -> ComponentF s f g p
componentF r e = component r (`bindF` e)

-- | Builds a new [`ComponentFC`](#componentfc) from a [`RenderFC`](#renderfc)
-- | and [`Eval`](#eval) function.
componentFC :: forall s f g p. (Functor g) => RenderFC s p f -> Eval f s g -> ComponentFC s f g p
componentFC r e = component r (`bindFC` e)

type ComponentState s f g p = Tuple s (Component s f g p)

type InstalledState s s' f' p p' g =
  { parent   :: s
  , children :: M.Map p (ComponentState s' f' g p')
  }

mapStateFParent :: forall s s' f f' p p' g. Natural (StateF s) (StateF (InstalledState s s' f' p p' g))
mapStateFParent (Get k) = Get (\st -> k st.parent)
mapStateFParent (Modify f next) = Modify (\st -> { parent: f st.parent, children: st.children }) next

mapStateFChild :: forall s s' f f' p p' g. (Ord p) => p -> Natural (StateF s') (StateF (InstalledState s s' f' p p' g))
mapStateFChild p (Get k) = Get (\st -> k $ U.fromJust $ fst <$> M.lookup p st.children)
mapStateFChild p (Modify f next) = Modify (\st -> { parent: st.parent, children: M.update (Just <<< lmap f) p st.children }) next

data ChildF p f i = ChildF p (f i)

instance functorChildF :: (Functor f) => Functor (ChildF p f) where
  map f (ChildF p fi) = ChildF p (f <$> fi)

installer :: forall s f g p s' f' p' q q'. (Ord p, Monad g, Plus g)
          => (q -> Either p q')
          -> (p' -> q')
          -> Component s f (QueryF s s' f' p p' g) q
          -> (p -> ComponentState s' f' g p')
          -> Component (InstalledState s s' f' p p' g) (Coproduct f (ChildF p f')) g q'
installer fromQ toQ' c f = Component { render: render, query: eval }
  where

  render :: State (InstalledState s s' f' p p' g) (HTML q' ((Coproduct f (ChildF p f')) Unit))
  render = do
    st <- CMS.get :: _ (InstalledState s s' f' p p' g)
    case renderComponent c st.parent of
      Tuple html s -> do
        -- Empty the state so that we don't keep children that are no longer
        -- being rendered...
        CMS.put { parent: s, children: M.empty :: M.Map p (ComponentState s' f' g p') }
        -- ...but then pass through the old state so we can lookup child
        -- components that are being re-rendered
        install (renderChild st) left html

  renderChild :: InstalledState s s' f' p p' g
              -> q
              -> State (InstalledState s s' f' p p' g) (HTML q' ((Coproduct f (ChildF p f')) Unit))
  renderChild st p = case fromQ p of
    Left p' -> renderChild' st p' $ case M.lookup p' st.children of
      Just sc -> sc
      Nothing -> f p'
    Right p' -> pure $ Placeholder p'

  renderChild' :: InstalledState s s' f' p p' g
               -> p
               -> ComponentState s' f' g p'
               -> State (InstalledState s s' f' p p' g) (HTML q' ((Coproduct f (ChildF p f')) Unit))
  renderChild' st p (Tuple s c) = case renderComponent c s of
    Tuple html s' -> do
      CMS.modify (\st -> st { children = M.insert p (Tuple s' c) st.children })
      pure $ lmap toQ' $ right <<< ChildF p <$> html

  eval :: Eval (Coproduct f (ChildF p f')) (InstalledState s s' f' p p' g) g
  eval = coproduct queryParent queryChild

  queryParent :: Eval f (InstalledState s s' f' p p' g) g
  queryParent q = bindF (queryComponent c q) (coproduct mergeParentStateF id)

  mergeParentStateF :: Natural (StateF s) (Free (Coproduct (StateF (InstalledState s s' f' p p' g)) g))
  mergeParentStateF sf = liftF $ left (mapStateFParent sf) :: Coproduct (StateF (InstalledState s s' f' p p' g)) g _

  queryChild :: Eval (ChildF p f') (InstalledState s s' f' p p' g) g
  queryChild (ChildF p q) = query p q >>= maybe empty' pure

empty' :: forall f g a. (Functor f, Plus g) => Free (Coproduct f g) a
empty' = liftF (right empty :: Coproduct f g a)

installL :: forall s f g pl pr s' f' p'. (Ord pl, Monad g, Plus g)
         => Component s f (QueryF s s' f' pl p' g) (Either pl pr)
         -> (pl -> ComponentState s' f' g p')
         -> Component (InstalledState s s' f' pl p' g) (Coproduct f (ChildF pl f')) g (Either p' pr)
installL = installer (either Left (Right <<< Right)) Left

installR :: forall s f g pl pr s' f' p'. (Ord pr, Monad g, Plus g)
         => Component s f (QueryF s s' f' pr p' g) (Either pl pr)
         -> (pr -> ComponentState s' f' g p')
         -> Component (InstalledState s s' f' pr p' g) (Coproduct f (ChildF pr f')) g (Either pl p')
installR = installer (either (Right <<< Left) Left) Right

installAll :: forall s f g p s' f' p'. (Ord p, Monad g, Plus g)
           => Component s f (QueryF s s' f' p p' g) p
           -> (p -> ComponentState s' f' g p')
           -> Component (InstalledState s s' f' p p' g) (Coproduct f (ChildF p f')) g p'
installAll = installer Left id

type QueryF s s' f' p p' g = Free (Coproduct (StateF (InstalledState s s' f' p p' g)) g)

query :: forall s s' f' p p' g i. (Functor g, Ord p) => p -> f' i -> QueryF s s' f' p p' g (Maybe i)
query p q = do
  st <- get :: _ (InstalledState s s' f' p p' g)
  case M.lookup p st.children of
    Nothing -> pure Nothing
    Just (Tuple _ c) -> Just <$> mapF (coproduct (left <<< mapStateFChild p) right) (queryComponent c q)

liftQuery :: forall s s' f' p p' g i. (Functor g) => QueryF s s' f' p p' g i -> Free (Coproduct (StateF s) (QueryF s s' f' p p' g)) i
liftQuery qf = liftF (right qf :: Coproduct (StateF s) (QueryF s s' f' p p' g) i)
