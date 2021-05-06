module Example.Driver.Websockets.Log where

import Prelude

import Data.Array as A
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event)
import Web.Event.Event as Event

data Query a = ReceiveMessage String a

data Message = OutputMessage String

data Action
  = HandleInput String
  | Submit Event

type State =
  { messages :: Array String
  , inputText :: String
  }

component :: forall i m. MonadEffect m => H.Component Query i Message m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        }
    }

initialState :: forall i. i -> State
initialState _ = { messages: [] , inputText: "" }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.form
    [ HE.onSubmit Submit ]
    [ HH.ol_ $ map (\msg -> HH.li_ [ HH.text msg ]) state.messages
    , HH.input
        [ HP.type_ HP.InputText
        , HP.value (state.inputText)
        , HE.onValueInput HandleInput
        ]
    , HH.button
        [ HP.type_ HP.ButtonSubmit ]
        [ HH.text "Send Message" ]
    ]

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action () Message m Unit
handleAction = case _ of
  HandleInput text -> do
    H.modify_ (_ { inputText = text })
  Submit ev -> do
    H.liftEffect $ Event.preventDefault ev
    st <- H.get
    let outgoingMessage = st.inputText
    H.raise $ OutputMessage outgoingMessage
    H.modify_ \st' -> st'
      { messages = st'.messages `A.snoc` ("Sending: " <> outgoingMessage)
      , inputText = ""
      }

handleQuery :: forall m a. Query a -> H.HalogenM State Action () Message m (Maybe a)
handleQuery = case _ of
  ReceiveMessage msg a -> do
    let incomingMessage = "Received: " <> msg
    H.modify_ \st -> st { messages = st.messages `A.snoc` incomingMessage }
    pure (Just a)
