module Main where

import Prelude (bind, Unit)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Random(RANDOM)
import Pux (start, renderToDOM, CoreEffects)
import Simon (initial, update, view, SOUND)

main :: forall e. Eff (console :: CONSOLE, random :: RANDOM, sound :: SOUND | CoreEffects e) Unit
main = do
  log "Simon!"
  app <- start 
    { initialState: initial 
    , update: update
    , view: view
    , inputs: []
    }

  renderToDOM "#app" app.html
