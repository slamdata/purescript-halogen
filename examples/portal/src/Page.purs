module Example.Portal.Page where

import Prelude

import Example.Portal.Modal (modal)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Web.HTML.HTMLElement (HTMLElement)

type Input = { target :: HTMLElement }

data Action = Open | Close

component :: forall q o m. H.Component q Input o m
component = H.mkComponent
  { initialState: \{ target } -> { target, open: false }
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  render { open, target } =
    HH.div_
      [ HH.button
          [ HE.onClick \_ -> Open ]
          [ HH.text "Show modal?"
          , modal
              { open
              , onClose: Close
              , target
              , children: HH.text """
                  Open the developer tools to verify this modal is rendered to <body>,
                  despite being within the <button> in the render function.
                """
              }
          ]
      ]

  handleAction = case _ of
    Open -> H.modify_ _ { open = true }
    Close -> H.modify_ _ { open = false }
