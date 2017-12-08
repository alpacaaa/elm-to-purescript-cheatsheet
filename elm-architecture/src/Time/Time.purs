module Time.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref as Ref
import Control.Monad.Eff.Timer as Timer
import Data.Const (Const)
import Data.Foldable (traverse_)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Time.Duration as Date
import Data.DateTime.Instant as Date
import Control.Monad.Eff.Now as Date
import Spork.App as App
import Spork.EventQueue as EventQueue
import Spork.Html (Html, div, text, h2)
import Spork.Interpreter (Interpreter(..), merge, never)


type Model =
    Maybe Date.Instant


data Msg
    = Tick Date.Instant


data Sub a =
    TickTime (Date.Instant -> a)


derive instance functorSub ∷ Functor Sub


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


subs :: Model -> App.Batch Sub Msg
subs model =
    App.lift (TickTime Tick)


app :: App.App (Const Void) Sub Model Msg
app =
    { render
    , update
    , subs
    , init
    }


runSubscriptions :: forall i. Interpreter (Eff _) Sub i
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