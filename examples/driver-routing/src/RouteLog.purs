module Example.Driver.Routing.RouteLog where

import Prelude

import Data.Array as A
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

data Query a = ChangeRoute String a

type State = { history :: Array String }

component :: forall i o m. H.Component Query i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleQuery = handleQuery }
    }

initialState :: forall i. i -> State
initialState _ = { history: [] }

render :: forall act m. State -> H.ComponentHTML act () m
render state =
  HH.div_
    [ HH.p_ [ HH.text "Change the URL hash or choose an anchor link..." ]
    , HH.ul_
        [ HH.li_ [ HH.a [ HP.href "#link-a" ] [ HH.text "Link A" ] ]
        , HH.li_ [ HH.a [ HP.href "#link-b" ] [ HH.text "Link B" ] ]
        , HH.li_ [ HH.a [ HP.href "#link-c" ] [ HH.text "Link C" ] ]
        ]
    , HH.p_ [ HH.text "...to see it logged below:" ]
    , HH.ol_ $ map (\msg -> HH.li_ [ HH.text msg ]) state.history
    ]

handleQuery :: forall act o m a. Query a -> H.HalogenM State act () o m (Maybe a)
handleQuery = case _ of
  ChangeRoute msg a -> do
    H.modify_ \st -> { history: st.history `A.snoc` msg }
    pure (Just a)
