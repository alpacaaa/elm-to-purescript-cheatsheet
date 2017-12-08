module Buttons.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Data.Const (Const)
import Data.Monoid (mempty)
import Spork.Html (Html, div, text, button, onClick)
import Spork.App (BasicApp)
import Spork.App as App
import Spork.Interpreter as Interpreter

type Model = Int

data Msg = Increment | Decrement

render :: Model -> Html Msg
render model =
    div []
        [ button [ onClick (const $ Just Decrement) ] [ text "-" ]
        , div [] [ text (show model) ]
        , button [ onClick (const $ Just Increment) ] [ text "+" ]
        ]


update :: Model -> Msg -> App.Transition (Const Void) Model Msg
update model msg =
    case msg of
        Increment -> App.purely (model + 1)
        Decrement -> App.purely (model - 1)

app :: BasicApp (Const Void) Model Msg
app =
    { render
    , update
    , subs: const mempty
    , init: App.purely 0
    }

-- This is probably the wrong way of going about this
noFx =
    (Interpreter.never `Interpreter.merge` Interpreter.never)

main :: Eff _ Unit
main = do
    inst <- App.makeWithSelector noFx app "#app"
    inst.run