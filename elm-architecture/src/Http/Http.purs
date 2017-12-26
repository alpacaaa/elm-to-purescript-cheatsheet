module Http.Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception as Exception
import Data.Argonaut as A
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Either as Either
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.StrMap as StrMap
import Network.HTTP.Affjax as Affjax
import Spork.App as App
import Spork.Html (Html, div, button, onClick, text, h2, img, src)
import Spork.Interpreter (merge, never, throughAff)



type Model =
    { topic :: String
    , gifUrl :: String
    }


data Msg
    = MorePlease
    | NewGif (Either String String)


data Effect a
    = GetRandomGif String (Either String String -> a)


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

        NewGif (Left error) ->
            App.purely model

        NewGif (Right newUrl) ->
            App.purely $ model { gifUrl = newUrl }


app :: App.App Effect (Const Void) Model Msg
app =
    { render
    , update
    , subs: const mempty
    , init
    }


decodeResponse :: A.Json -> Either String String
decodeResponse json =
    A.toObject json
    >>= StrMap.lookup "data"
    >>= A.toObject
    >>= StrMap.lookup "image_url"
    >>= A.toString
    # Either.note "Unable to decode"


runEffect :: forall eff. Effect ~> Aff _
runEffect effect =
    case effect of
        GetRandomGif topic next -> do
            let url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" <> topic
            res <- Affjax.get url
            let decoded = decodeResponse res.response
            pure (next decoded)


handleErrors :: Exception.Error -> Eff _ Unit
handleErrors error =
    -- TODO what is this?
    pure unit


main :: Eff _ Unit
main = do
    let interpreter = throughAff runEffect handleErrors
    inst <- App.makeWithSelector (interpreter `merge` never) app "#app"
    inst.run