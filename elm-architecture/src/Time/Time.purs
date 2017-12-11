module Time.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref as Ref
import Control.Monad.Eff.Timer as Timer
import Data.Const (Const)
import Data.Foldable (traverse_)
import Math as Math
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration as Date
import Data.DateTime.Instant as Date
import Control.Monad.Eff.Now as Date
import Spork.App as App
import Spork.EventQueue as EventQueue
import Spork.Html (Html, div)
import Spork.Html as Html
import Spork.Interpreter (Interpreter(..), merge, never)


-- utter crap
-- requires patch in Spork/Html/Core

{--
import Halogen.HTML.Core (Prop(Attribute), Namespace(Namespace), AttrName(AttrName))
import Halogen.HTML.Core as H

ns :: Maybe Namespace
ns = Just $ Namespace "http://www.w3.org/2000/svg"

elemSvg ∷ ∀ r i. String → Array (IProp r i) → Array (Html i) → Html i
elemSvg name props children =
    Html
        (V.Elem
            (V.ElemSpec ns (V.ElemName name) (unsafeCoerce props ∷ Array (P.Prop i)))
            (unwrapF children))
--}

svg = Html.elemSvg "svg"
circle = Html.elemSvg "circle"
line = Html.elemSvg "line"

viewBox = Html.attr "viewBox"
cx = Html.attr "cx"
cy = Html.attr "cy"
r = Html.attr "r"
fill = Html.attr "fill"

x1 = Html.attr "x1"
x2 = Html.attr "x2"
y1 = Html.attr "y1"
y2 = Html.attr "y2"
stroke = Html.attr "stroke"

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
    div [ Html.styles [Html.Style "width" "300px"] ]
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