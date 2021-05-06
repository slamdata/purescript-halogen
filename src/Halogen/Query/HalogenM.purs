module Halogen.Query.HalogenM where

import Prelude

import Control.Applicative.Free (FreeAp, liftFreeAp, hoistFreeAp)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Free (Free, hoistFree, liftF)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans)
import Control.Monad.Writer.Class (class MonadTell, tell)
import Control.Parallel.Class (class Parallel)
import Data.Bifunctor (lmap)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, over)
import Data.Symbol (class IsSymbol)
import Data.Traversable (traverse)
import Data.Tuple (Tuple)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Data.Slot (Slot)
import Halogen.Data.Slot as Slot
import Halogen.Query.ChildQuery as CQ
import Halogen.Query.Input (RefLabel)
import Halogen.Subscription as HS
import Prim.Row as Row
import Type.Proxy (Proxy)
import Web.DOM (Element)

-- | The Halogen component eval algebra.
-- |
-- | - `state` is the component's state
-- | - `action` is the type of actions; events internal to the component that
-- |   can be evaluated
-- | - `slots` is the set of slots for addressing child components
-- | - `output` is the type of output messages the component can raise
-- | - `m` is the monad used during evaluation
-- | - `a` is the result of the HalogenF expression (see HalogenM for an example).
data HalogenF state action slots output m a
  = State (state -> Tuple a state)
  | Subscribe (SubscriptionId -> HS.Emitter action) (SubscriptionId -> a)
  | Unsubscribe SubscriptionId a
  | Lift (m a)
  | ChildQuery (CQ.ChildQueryBox slots a)
  | Raise output a
  | Par (HalogenAp state action slots output m a)
  | Fork (HalogenM state action slots output m Unit) (ForkId -> a)
  | Kill ForkId a
  | GetRef RefLabel (Maybe Element -> a)

instance functorHalogenF :: Functor m => Functor (HalogenF state action slots output m) where
  map f = case _ of
    State k -> State (lmap f <<< k)
    Subscribe fes k -> Subscribe fes (f <<< k)
    Unsubscribe sid a -> Unsubscribe sid (f a)
    Lift q -> Lift (map f q)
    ChildQuery cq -> ChildQuery (map f cq)
    Raise o a -> Raise o (f a)
    Par pa -> Par (map f pa)
    Fork hmu k -> Fork hmu (f <<< k)
    Kill fid a -> Kill fid (f a)
    GetRef p k -> GetRef p (f <<< k)

-- | The Halogen component eval effect monad.
-- |
-- | - `state` is the component's state
-- | - `action` is the type of actions; veents internal to the component that
-- |   can be evaluated
-- | - `slots` is the set of slots for addressing child components
-- | - `output` is the type of output messages the component can raise
-- | - `m` is the monad used during evaluation
-- | - `a` is the result of the HalogenM expression. Use the following pattern:
-- |     `handleAction :: Action -> H.HalogenM State Action Slots Output m Unit`
-- |     `handleQuery  :: forall a. Query a -> H.HalogenM State Action Slots Output m (Maybe a)`
newtype HalogenM state action slots output m a = HalogenM (Free (HalogenF state action slots output m) a)

derive newtype instance functorHalogenM :: Functor (HalogenM state action slots output m)
derive newtype instance applyHalogenM :: Apply (HalogenM state action slots output m)
derive newtype instance applicativeHalogenM :: Applicative (HalogenM state action slots output m)
derive newtype instance bindHalogenM :: Bind (HalogenM state action slots output m)
derive newtype instance monadHalogenM :: Monad (HalogenM state action slots output m)
derive newtype instance semigroupHalogenM :: Semigroup a => Semigroup (HalogenM state action slots output m a)
derive newtype instance monoidHalogenM :: Monoid a => Monoid (HalogenM state action slots output m a)

instance monadEffectHalogenM :: MonadEffect m => MonadEffect (HalogenM state action slots output m) where
  liftEffect = HalogenM <<< liftF <<< Lift <<< liftEffect

instance monadAffHalogenM :: MonadAff m => MonadAff (HalogenM state action slots output m) where
  liftAff = HalogenM <<< liftF <<< Lift <<< liftAff

instance parallelHalogenM :: Parallel (HalogenAp state action slots output m) (HalogenM state action slots output m) where
  parallel = HalogenAp <<< liftFreeAp
  sequential = HalogenM <<< liftF <<< Par

instance monadTransHalogenM :: MonadTrans (HalogenM state action slots o) where
  lift = HalogenM <<< liftF <<< Lift

instance monadRecHalogenM :: MonadRec (HalogenM state action slots output m) where
  tailRecM k a = k a >>= case _ of
    Loop x -> tailRecM k x
    Done y -> pure y

instance monadStateHalogenM :: MonadState state (HalogenM state action slots output m) where
  state = HalogenM <<< liftF <<< State

instance monadAskHalogenM :: MonadAsk r m => MonadAsk r (HalogenM state action slots output m) where
  ask = HalogenM $ liftF $ Lift ask

instance monadTellHalogenM :: MonadTell w m => MonadTell w (HalogenM state action slots output m) where
  tell = HalogenM <<< liftF <<< Lift <<< tell

instance monadThrowHalogenM :: MonadThrow e m => MonadThrow e (HalogenM state action slots output m) where
  throwError = HalogenM <<< liftF <<< Lift <<< throwError

-- | An applicative-only version of `HalogenM` to allow for parallel evaluation.
newtype HalogenAp state action slots output m a = HalogenAp (FreeAp (HalogenM state action slots output m) a)

derive instance newtypeHalogenAp :: Newtype (HalogenAp state query slots output m a) _
derive newtype instance functorHalogenAp :: Functor (HalogenAp state query slots output m)
derive newtype instance applyHalogenAp :: Apply (HalogenAp state query slots output m)
derive newtype instance applicativeHalogenAp :: Applicative (HalogenAp state query slots output m)

-- | Raises an output message for the component.
raise :: forall state action slots output m. output -> HalogenM state action slots output m Unit
raise o = HalogenM $ liftF $ Raise o unit

-- | Sends a query to a child of a component at the specified slot.
query
  :: forall state action output m label slots query output' slot a _1
   . Row.Cons label (Slot query output' slot) _1 slots
  => IsSymbol label
  => Ord slot
  => Proxy label
  -> slot
  -> query a
  -> HalogenM state action slots output m (Maybe a)
query label p q = HalogenM $ liftF $ queryF label p q

-- | An alternative to `query` which does not wrap the HalogenF in free
queryF
  :: forall state action output m label slots query output' slot a _1
   . Row.Cons label (Slot query output' slot) _1 slots
  => IsSymbol label
  => Ord slot
  => SProxy label
  -> slot
  -> query a
  -> HalogenF state action slots output m (Maybe a)
queryF label p q = ChildQuery $ CQ.mkChildQueryBox $
  CQ.ChildQuery (\k → maybe (pure Nothing) k <<< Slot.lookup label p) q identity

-- | Sends a query to all children of a component at a given slot label.
queryAll
  :: forall state action output m label slots query output' slot a _1
   . Row.Cons label (Slot query output' slot) _1 slots
  => IsSymbol label
  => Ord slot
  => Proxy label
  -> query a
  -> HalogenM state action slots output m (Map slot a)
queryAll label q =
  HalogenM $ liftF $ ChildQuery $ CQ.mkChildQueryBox $
    CQ.ChildQuery (\k -> map catMapMaybes <<< traverse k <<< Slot.slots label) q identity
  where
    catMapMaybes ∷ forall k v. Ord k ⇒ Map k (Maybe v) -> Map k v
    catMapMaybes = foldrWithIndex (\k v acc → maybe acc (flip (Map.insert k) acc) v) Map.empty

-- | The ID value associated with a subscription. Allows the subscription to be
-- | stopped at a later time.
newtype SubscriptionId = SubscriptionId Int

derive newtype instance eqSubscriptionId :: Eq SubscriptionId
derive newtype instance ordSubscriptionId :: Ord SubscriptionId

-- | Subscribes a component to an `Emitter`.
-- |
-- | When a component is disposed of any active subscriptions will automatically
-- | be stopped and no further subscriptions will be possible during
-- | finalization.
subscribe :: forall state action slots output m. HS.Emitter action -> HalogenM state action slots output m SubscriptionId
subscribe es = HalogenM $ liftF $ Subscribe (\_ -> es) identity

-- | An alternative to `subscribe`, intended for subscriptions that unsubscribe
-- | themselves. Instead of returning the `SubscriptionId` from `subscribe'`, it
-- | is passed into an `Emitter` constructor. This allows emitted queries
-- | to include the `SubscriptionId`, rather than storing it in the state of the
-- | component.
-- |
-- | When a component is disposed of any active subscriptions will automatically
-- | be stopped and no further subscriptions will be possible during
-- | finalization.
subscribe' :: forall state action slots output m. (SubscriptionId -> HS.Emitter action) -> HalogenM state action slots output m Unit
subscribe' esc = HalogenM $ liftF $ Subscribe esc (const unit)

-- | Unsubscribes a component from a subscription. If the subscription associated
-- | with the ID has already ended this will have no effect.
unsubscribe :: forall state action slots output m. SubscriptionId -> HalogenM state action slots output m Unit
unsubscribe sid = HalogenM $ liftF $ Unsubscribe sid unit

-- | The ID value associated with a forked process. Allows the fork to be killed
-- | at a later time.
newtype ForkId = ForkId Int

derive newtype instance eqForkId :: Eq ForkId
derive newtype instance ordForkId :: Ord ForkId

-- | Starts a `HalogenM` process running independent from the current `eval`
-- | "thread".
-- |
-- | A commonly use case for `fork` is in component initializers where some
-- | async action is started. Normally all interaction with the component will
-- | be blocked until the initializer completes, but if the async action is
-- | `fork`ed instead, the initializer can complete synchronously while the
-- | async action continues.
-- |
-- | Some care needs to be taken when using a `fork` that can modify the
-- | component state, as it's easy for the forked process to "clobber" the state
-- | (overwrite some or all of it with an old value) by mistake.
-- |
-- | When a component is disposed of any active forks will automatically
-- | be killed. New forks can be started during finalization but there will be
-- | no means of killing them.
fork :: forall state action slots output m. HalogenM state action slots output m Unit -> HalogenM state action slots output m ForkId
fork hmu = HalogenM $ liftF $ Fork hmu identity

-- | Kills a forked process if it is still running. Attempting to kill a forked
-- | process that has already ended will have no effect.
kill :: forall state action slots output m. ForkId -> HalogenM state action slots output m Unit
kill fid = HalogenM $ liftF $ Kill fid unit

-- | Retrieves an `Element` value that is associated with a `Ref` in the
-- | rendered output of a component. If there is no currently rendered value for
-- | the requested ref this will return `Nothing`.
getRef :: forall state action slots output m. RefLabel -> HalogenM state action slots output m (Maybe Element)
getRef p = HalogenM $ liftF $ GetRef p identity

imapState
  :: forall state state' action slots output m
   . (state -> state')
  -> (state' -> state)
  -> HalogenM state action slots output m
  ~> HalogenM state' action slots output m
imapState f f' (HalogenM h) = HalogenM (hoistFree go h)
  where
  go :: HalogenF state action slots output m ~> HalogenF state' action slots output m
  go = case _ of
    State fs -> State (map f <<< fs <<< f')
    Subscribe fes k -> Subscribe fes k
    Unsubscribe sid a -> Unsubscribe sid a
    Lift q -> Lift q
    ChildQuery cq -> ChildQuery cq
    Raise o a -> Raise o a
    Par p -> Par (over HalogenAp (hoistFreeAp (imapState f f')) p)
    Fork hmu k -> Fork (imapState f f' hmu) k
    Kill fid a -> Kill fid a
    GetRef p k -> GetRef p k

mapAction
  :: forall state action action' slots output m
   . Functor m
  => (action -> action')
  -> HalogenM state action slots output m
  ~> HalogenM state action' slots output m
mapAction f (HalogenM h) = HalogenM (hoistFree go h)
  where
  go :: HalogenF state action slots output m ~> HalogenF state action' slots output m
  go = case _ of
    State fs -> State fs
    Subscribe fes k -> Subscribe (map f <<< fes) k
    Unsubscribe sid a -> Unsubscribe sid a
    Lift q -> Lift q
    ChildQuery cq -> ChildQuery cq
    Raise o a -> Raise o a
    Par p -> Par (over HalogenAp (hoistFreeAp (mapAction f)) p)
    Fork hmu k -> Fork (mapAction f hmu) k
    Kill fid a -> Kill fid a
    GetRef p k -> GetRef p k

mapOutput
  :: forall state action slots output output' m
   . (output -> output')
  -> HalogenM state action slots output m
  ~> HalogenM state action slots output' m
mapOutput f (HalogenM h) = HalogenM (hoistFree go h)
  where
  go :: HalogenF state action slots output m ~> HalogenF state action slots output' m
  go = case _ of
    State fs -> State fs
    Subscribe fes k -> Subscribe fes k
    Unsubscribe sid a -> Unsubscribe sid a
    Lift q -> Lift q
    ChildQuery cq -> ChildQuery cq
    Raise o a -> Raise (f o) a
    Par p -> Par (over HalogenAp (hoistFreeAp (mapOutput f)) p)
    Fork hmu k -> Fork (mapOutput f hmu) k
    Kill fid a -> Kill fid a
    GetRef p k -> GetRef p k

hoist
  :: forall state action slots output m m'
   . Functor m'
  => (m ~> m')
  -> HalogenM state action slots output m
  ~> HalogenM state action slots output m'
hoist nat (HalogenM fa) = HalogenM (hoistFree go fa)
  where
  go :: HalogenF state action slots output m ~> HalogenF state action slots output m'
  go = case _ of
    State f -> State f
    Subscribe fes k -> Subscribe fes k
    Unsubscribe sid a -> Unsubscribe sid a
    Lift q -> Lift (nat q)
    ChildQuery cq -> ChildQuery cq
    Raise o a -> Raise o a
    Par p -> Par (over HalogenAp (hoistFreeAp (hoist nat)) p)
    Fork hmu k -> Fork (hoist nat hmu) k
    Kill fid a -> Kill fid a
    GetRef p k -> GetRef p k
