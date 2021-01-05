module Example.Portal.Modal where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.HTML.HTMLElement (HTMLElement)

type ModalInput action slots m =
  { children :: H.ComponentHTML action slots m
  , onClose :: action
  , open :: Boolean
  , target :: HTMLElement
  }

modal
 :: forall action slots m
  . ModalInput action slots m
 -> H.ComponentHTML action slots m
modal { open, onClose, target, children } =
  if open then
    HH.portal target do
      HH.div
        [ HP.class_ $ HH.ClassName "modal" ]
        [ HH.button
            [ HP.class_ $ HH.ClassName "modal__close"
            , HE.onClick \_ -> onClose
            ]
            [ HH.text "✕" ]
        , children
        ]
  else HH.text ""
