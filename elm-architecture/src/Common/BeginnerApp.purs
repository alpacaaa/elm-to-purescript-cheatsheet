module Common.BeginnerApp
    ( BeginnerApp
    , makeWithSelector
    ) where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Const (Const)
import Data.Monoid (mempty)
import Spork.App as App
import Spork.Html (Html)
import Spork.Interpreter as Interpreter

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
    -> App.BasicApp (Const Void) model action
makeBeginnerApp app =
    { render: app.render
    , update: beginnerUpdate app.update
    , subs:   const mempty
    , init:   App.purely app.model
    }

-- This is probably the wrong way of going about this
noFx =
    (Interpreter.never `Interpreter.merge` Interpreter.never)

makeWithSelector
    :: forall model action eff
    .  BeginnerApp model action
    -> String
    -> Eff (App.AppEffects eff) (App.AppInstance (App.AppEffects eff) model action)
makeWithSelector app selector =
    App.makeWithSelector noFx (makeBeginnerApp app) selector