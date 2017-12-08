module TextFields.Main where

import Prelude

import Common.BeginnerApp (BeginnerApp)
import Common.BeginnerApp as BeginnerApp
import Control.Monad.Eff (Eff)
import Data.Array as Array
import Data.String as String
import Spork.Html (Html, div, input, onValueInput, placeholder, text, always)



type Model =
    { content :: String
    }


data Msg
    = Change String


model :: Model
model =
    { content: "" }


render :: Model -> Html Msg
render model =
    div []
        [ input [ placeholder "Text to reverse", onValueInput (always Change) ]
        , div [] [ text (reverse model.content) ]
        ]


update :: Model -> Msg -> Model
update model msg =
    case msg of
        Change newContent ->
            model { content = newContent }


app :: BeginnerApp Model Msg
app =
    { render
    , update
    , model
    }


reverse :: String -> String
reverse str =
    String.toCharArray str
    # Array.reverse
    # String.fromCharArray


main :: Eff _ Unit
main = do
    inst <- BeginnerApp.makeWithSelector app "#app"
    inst.run