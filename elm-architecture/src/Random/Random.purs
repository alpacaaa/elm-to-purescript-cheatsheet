module Random.Main where

import Prelude

import Effect.Random as Random
import Effect(Effect)
import Data.Maybe(Maybe(..))
import Data.Const (Const)
import Spork.App as App
import Spork.Html (Html, div, button, onClick, text, h1) as H
import Spork.Interpreter (liftNat, merge, never)

type Model =
    { dieFace :: Int
    }

data Msg
    = Roll
    | NewFace Int

data Unpure a
    = GenerateRandom (Int -> a)

init :: App.Transition Unpure Model Msg
init =
    App.purely { dieFace: 1 }

render :: Model -> H.Html Msg
render model =
    H.div []
        [ H.h1 [] [ H.text (show model.dieFace) ]
        , H.button [ H.onClick (const $ Just Roll) ] [ H.text "Roll" ]
        ]

update :: Model -> Msg -> App.Transition Unpure Model Msg
update model msg =
    case msg of
        Roll ->
            { model, effects: App.lift (GenerateRandom NewFace) }

        NewFace newFace ->
            App.purely $ model { dieFace = newFace }

app :: App.App Unpure (Const Void) Model Msg
app =
    { render
    , update
    , subs: const mempty
    , init
    }

runEffect ::  forall a. Unpure a -> Effect a
runEffect effect =
    case effect of
        GenerateRandom next -> do
            n <- Random.randomInt 1 6
            pure (next n)

main :: Effect Unit
main = do
    inst <- App.makeWithSelector (liftNat runEffect `merge` never) app "#app"
    inst.run