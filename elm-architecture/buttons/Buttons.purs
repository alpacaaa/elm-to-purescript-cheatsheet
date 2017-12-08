module Buttons.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Const (Const)
import Data.Monoid (mempty)
import Spork.Html (Html, div, text)
import Spork.App (BasicApp)
import Spork.App as App
import Spork.Interpreter as Interpreter

type Model =
    Unit

data Msg

render :: Model -> Html Msg
render model =
    div [] [text "YOLO"]


update :: Model -> Msg -> App.Transition (Const Void) Model Msg
update model msg =
    App.purely model

app :: BasicApp (Const Void) Model Msg
app =
    { render
    , update
    , subs: const mempty
    , init: App.purely unit
    }

-- This is probably the wrong way of going about this
noFx =
    (Interpreter.never `Interpreter.merge` Interpreter.never)

main :: Eff _ Unit
main = do
    inst <- App.makeWithSelector noFx app "#app"
    inst.run