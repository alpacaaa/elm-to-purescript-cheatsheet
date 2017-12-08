module Http.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Maybe
import Data.Monoid (mempty)
import Data.Const (Const)
import Spork.App as App
import Spork.Html (Html, div, button, onClick, text, h2, img, src)
import Spork.Interpreter (liftNat, merge, never)



type Model =
    { topic :: String
    , gifUrl :: String
    }


data Msg
    = MorePlease
    | NewGif String


data Effect a
    = GetRandomGif String (String -> a)


init :: App.Transition Effect Model Msg
init =
    App.purely { topic: "cats", gifUrl: "" }


render :: Model -> Html Msg
render model =
    div []
        [ h2 [] [text model.topic]
        , img [src model.gifUrl]
        , button [ onClick (const $ Just MorePlease) ] [ text "More Please!" ]
        ]


update :: Model -> Msg -> App.Transition Effect Model Msg
update model msg =
    case msg of
        MorePlease ->
            { model, effects: App.lift (GetRandomGif model.topic NewGif) }

        NewGif newUrl ->
            App.purely $ model { gifUrl = newUrl }


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
        GetRandomGif topic next -> do
            pure (next "Yo")


main :: Eff _ Unit
main = do
    inst <- App.makeWithSelector (liftNat runEffect `merge` never) app "#app"
    inst.run