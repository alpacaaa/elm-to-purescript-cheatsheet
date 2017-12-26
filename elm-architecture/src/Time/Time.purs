module Time.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref as Ref
import Control.Monad.Eff.Timer as Timer
import Data.Const (Const)
import Data.Foldable (traverse_)
import Math as Math
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (Minutes(..), convertDuration) as Date
import Data.DateTime.Instant (Instant, unInstant) as Date
import Control.Monad.Eff.Now (now) as Date
import Spork.App as App
import Spork.EventQueue as EventQueue
import Spork.Html (Html, div, styles, Style(..))
import Spork.Interpreter (Interpreter(..), merge, never)

import Common.Svg (svg, viewBox, circle, cx, cy, r, fill, line, x1, x2, y1, y2, stroke)

type Model =
    Maybe Date.Instant


data Msg
    = Tick Date.Instant


data Sub a =
    TickTime (Date.Instant -> a)


derive instance functorSub ∷ Functor Sub


init :: App.Transition (Const Void) Model Msg
init =
    App.purely Nothing


turns :: Number -> Number
turns ts =
    2.0 * Math.pi * ts


inMinutes :: Date.Instant -> Date.Minutes
inMinutes =
    Date.convertDuration <<< Date.unInstant


render :: Model -> Html Msg
render model =
    let
        Date.Minutes minutes =
            maybe (Date.Minutes 0.0) inMinutes model

        angle =
            turns minutes

        handX =
            show (50.0 + 40.0 * Math.cos angle)

        handY =
            show (50.0 + 40.0 * Math.sin angle)
    in
    div [ styles [Style "width" "300px"] ]
        [ svg [ viewBox "0 0 100 100"]
            [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
            , line [ x1 "50", y1 "50", x2 handX, y2 handY, stroke "#023963" ] []
            ]
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
                TickTime k -> queue.push (k now)
            queue.run

        commit new = do
            old <- Ref.readRef model
            Ref.writeRef model new
            case old, new of
                [], _ -> void $ Timer.setInterval 1000 tick
                _, _  -> pure unit

            pure unit

    pure commit


main :: Eff _ Unit
main = do
    inst <- App.makeWithSelector (never `merge` runSubscriptions) app "#app"
    inst.run
