module Common.Svg where

import Prelude
import Data.Maybe (Maybe(..))
import Spork.Html (IProp, Html)
import Spork.Html as Html

type Svg =
    forall r i. Array (IProp r i) -> Array (Html i) -> Html i 

elemSvg :: forall r i. String -> Svg
elemSvg =
    Html.elemWithNS (Just $ Html.Namespace "http://www.w3.org/2000/svg")

svg :: Svg
svg = elemSvg "svg"

circle :: Svg
circle = elemSvg "circle"

line :: Svg
line = elemSvg "line"

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