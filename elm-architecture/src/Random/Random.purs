module Random.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random as Random
import Data.Maybe
import Data.Monoid (mempty)
import Data.Const (Const)
import Spork.App as App
import Spork.Html (Html, div, button, onClick, text, h1)
import Spork.Interpreter (liftNat, merge, never)



type Model =
    { dieFace :: Int
    }


data Msg
    = Roll
    | NewFace Int


data Effect a
    = GenerateRandom (Int -> a)


init :: App.Transition Effect Model Msg
init =
    App.purely { dieFace: 1 }


render :: Model -> Html Msg
render model =
    div []
        [ h1 [] [ text (show model.dieFace) ]
        , button [ onClick (const $ Just Roll) ] [ text "Roll" ]
        ]


update :: Model -> Msg -> App.Transition Effect Model Msg
update model msg =
    case msg of
        Roll ->
            { model, effects: App.lift (GenerateRandom NewFace) }

        NewFace newFace ->
            App.purely $ model { dieFace = newFace }


app :: App.App Effect (Const Void) Model Msg
app =
    { render
    , update
    , subs: const mempty
    , init
    }


runEffect :: forall eff. Effect ~> Eff _
runEffect effect =
    case effect of
        GenerateRandom next -> do
            n <- Random.randomInt 1 6
            pure (next n)


main :: Eff _ Unit
main = do
    inst <- App.makeWithSelector (liftNat runEffect `merge` never) app "#app"
    inst.run