module Magnet exposing (..)

import Collage exposing (..)
import Collage.Text as Text
import Collage.Layout as Layout
import Color


type alias Magnet a =
    { data : a
    , text : String
    , position : ( Float, Float )
    , padding : { width : Float, height : Float }
    }


magnet : String -> ( Float, Float ) -> Magnet ()
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
