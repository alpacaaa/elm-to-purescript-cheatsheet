module Time.Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception as Exception
import Control.Monad.Eff.Ref as Ref
import Control.Monad.Eff.Timer as Timer
import Data.Argonaut as A
import Data.Const (Const)
import Data.DateTime.Instant as Date
import Data.Either (Either(..))
import Data.Either as Either
import Data.Foldable (traverse_)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.StrMap as StrMap
import Data.Time.Duration as Date
import Control.Monad.Eff.Now as Date
import Spork.App as App
import Spork.EventQueue as EventQueue
import Spork.Html (Html, div, button, onClick, text, h2, img, src)
import Spork.Interpreter (Interpreter(..))
import Spork.Interpreter (merge, never, throughAff)


type Model =
    Maybe Date.Instant


data Msg
    = Tick Date.Instant


init :: App.Transition (Const Void) Model Msg
init =
    App.purely (Date.instant $ Date.Milliseconds $ Int.toNumber 0)


render :: Model -> Html Msg
render model =
    div []
        [ h2 [] [text (show model)]
        ]


update :: Model -> Msg -> App.Transition (Const Void) Model Msg
update model msg =
    case msg of
        Tick time ->
            App.purely (Just time)


data Sub a =
    TickTime (Date.Instant -> a)

derive instance functorSub ∷ Functor Sub

subs model =
    App.lift (TickTime Tick)


app :: App.App (Const Void) Sub Model Msg
app =
    { render
    , update
    , subs
    , init
    }


runSubscriptions = Interpreter $ EventQueue.withAccumArray \queue -> do
    model <- Ref.newRef []

    let
        tick = do
            now <- Date.now
            Ref.readRef model >>= traverse_ case _ of
                TickTime k → queue.push (k now)
            queue.run

        commit new = do
            old <- Ref.readRef model
            Ref.writeRef model new
            case old, new of
                [], _ -> void $ Timer.setInterval 500 tick
                _, _  -> pure unit

            pure unit

    pure commit


main :: Eff _ Unit
main = do
    inst <- App.makeWithSelector (never `merge` runSubscriptions) app "#app"
    inst.run