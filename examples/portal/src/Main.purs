module Example.Portal.Main where

import Prelude

import Effect (Effect)
import Example.Portal.Page as Page
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Page.component { target: body } body
