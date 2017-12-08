module Common.BeginnerApp
    ( BeginnerApp
    , makeWithSelector
    ) where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Const (Const)
import Data.Monoid (mempty)
import Spork.App (BasicApp, AppEffects, AppInstance)
import Spork.Html (Html)
import Spork.App as App
import Spork.Interpreter as Interpreter

-- This is probably the wrong way of going about this
noFx =
    (Interpreter.never `Interpreter.merge` Interpreter.never)

type BeginnerApp model action =
    { model  :: model
    , render :: model -> Html action
    , update :: model -> action -> model
    }


beginnerUpdate
    :: forall model action
    .  (model -> action -> model)
    -> model
    -> action
    -> App.Transition (Const Void) model action
beginnerUpdate fn model action =
    App.purely (fn model action)


makeBeginnerApp
    :: forall model action
    .  BeginnerApp model action
    -> BasicApp (Const Void) model action
makeBeginnerApp app =
    { render: app.render
    , update: beginnerUpdate app.update
    , subs: const mempty
    , init: App.purely app.model
    }

makeWithSelector
    :: forall model action
    .  BeginnerApp model action
    -> String
    -> Eff (AppEffects ()) (AppInstance (AppEffects ()) model action)
makeWithSelector app selector =
    App.makeWithSelector noFx (makeBeginnerApp app) selector