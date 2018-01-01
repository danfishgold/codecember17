module Magnet exposing (..)

import Collage exposing (..)
import Collage.Text as Text
import Collage.Layout as Layout
import Color
import Point


type alias Point =
    Point.Point


type alias Magnet a =
    { data : a
    , text : String
    , position : Point
    , padding : { width : Float, height : Float }
    }


magnet : String -> Point -> Magnet ()
magnet text position =
    { data = ()
    , text = text
    , position = position
    , padding = { width = 10, height = 10 }
    }


view : Magnet a -> Collage msg
view { text, position, padding } =
    let
        textNode =
            text
                |> Text.fromString
                |> Text.color Color.white
                |> rendered

        bgNode =
            rectangle
                (Layout.width textNode + padding.width)
                (Layout.height textNode + padding.height)
                |> filled (uniform Color.black)
    in
        group
            [ textNode, bgNode ]
            |> shift position


size : Magnet a -> { width : Float, height : Float }
size { text, padding } =
    let
        textNode =
            text
                |> Text.fromString
                |> Text.color Color.white
                |> rendered
    in
        { width = Layout.width textNode + padding.width
        , height = Layout.height textNode + padding.height
        }


edges : Magnet a -> { minX : Float, maxX : Float, minY : Float, maxY : Float }
edges magnet =
    let
        { width, height } =
            size magnet

        ( x0, y0 ) =
            magnet.position
    in
        { minX = x0 - width / 2
        , maxX = x0 + width / 2
        , minY = y0 - height / 2
        , maxY = y0 + height / 2
        }


contains : Point -> Magnet a -> Bool
contains ( x, y ) magnet =
    let
        m =
            edges magnet
    in
        m.minX <= x && x <= m.maxX && m.minY <= y && y <= m.maxY


moveBy : Point -> Magnet a -> Magnet a
moveBy delta magnet =
    { magnet | position = Point.add magnet.position delta }
