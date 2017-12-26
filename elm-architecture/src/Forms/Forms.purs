module Forms.Main where

import Prelude

import Common.BeginnerApp (BeginnerApp)
import Common.BeginnerApp as BeginnerApp
import Control.Monad.Eff (Eff)
import Data.Tuple (Tuple(..))
import Spork.Html as H
import Spork.Html (Html, div, input, onValueInput, placeholder, text, always, type_, styles)



type Model =
    { name :: String
    , password :: String
    , passwordAgain :: String
    }


data Msg
    = Name String
    | Password String
    | PasswordAgain String


model :: Model
model =
    { name: "", password: "", passwordAgain: "" }


render :: Model -> Html Msg
render model =
    div []
        [ input [ type_ H.InputText, placeholder "Name", onValueInput (always Name) ]
        , input [ type_ H.InputPassword, placeholder "Password", onValueInput (always Password) ]
        , input [ type_ H.InputPassword, placeholder "Re-enter Password", onValueInput (always PasswordAgain) ]
        , viewValidation model
        ]


viewValidation :: forall msg. Model -> Html msg
viewValidation model =
    let
        Tuple color message =
            if model.password == model.passwordAgain then
                Tuple "green" "OK"
            else
                Tuple "red" "Passwords do not match!"
    in
    div [ styles [ H.Style "color" color ] ] [ text message ]


update :: Model -> Msg -> Model
update model msg =
    case msg of
        Name name ->
            model { name = name }

        Password password ->
            model { password = password }

        PasswordAgain password ->
            model { passwordAgain = password }


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