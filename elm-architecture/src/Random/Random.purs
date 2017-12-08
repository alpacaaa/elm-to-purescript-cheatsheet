module Random.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Maybe
import Data.Monoid (mempty)
import Data.Const (Const)
import Spork.App as App
import Spork.Html as H
import Spork.Html (Html, div, button, onClick, text, h1)
import Spork.Interpreter (liftNat, merge, never)



type Model =
    { dieFace :: Int
    }


data Msg
    = Roll

data Effect a
    = GenerateRandom a

init :: Model
init =
    { dieFace: 1 }


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
            App.purely model


app :: App.App Effect (Const Void) Model Msg
app =
  { render
  , update
  , subs: const mempty
  , init: App.purely init
  }


runEffect :: forall eff. Effect ~> Eff _
runEffect effect =
    case effect of
        GenerateRandom next ->
            pure next


main :: Eff _ Unit
main = do
    inst <- App.makeWithSelector (liftNat runEffect `merge` never) app "#app"
    inst.run