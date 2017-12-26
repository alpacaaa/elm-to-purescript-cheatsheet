module Buttons.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Spork.Html (Html, div, text, button, onClick)
import Common.BeginnerApp (BeginnerApp)
import Common.BeginnerApp as BeginnerApp



type Model =
    Int

data Msg
    = Increment | Decrement


model :: Model
model =
    0


render :: Model -> Html Msg
render model =
    div []
        [ button [ onClick (const $ Just Decrement) ] [ text "-" ]
        , div [] [ text (show model) ]
        , button [ onClick (const $ Just Increment) ] [ text "+" ]
        ]


update :: Model -> Msg -> Model
update model msg =
    case msg of
        Increment -> model + 1
        Decrement -> model - 1


app :: BeginnerApp Model Msg
app =
    { render
    , update
    , model
    }


main :: Eff _ Unit
main = do
    inst <- BeginnerApp.makeWithSelector app "#app"
    inst.run