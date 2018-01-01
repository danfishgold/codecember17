module Magnet exposing (..)

import Collage exposing (..)
import Collage.Text as Text
import Collage.Layout as Layout
import Color


type alias Magnet a =
    { data : a
    , text : String
    , position : ( Float, Float )
    }


view : Magnet a -> Collage msg
view magnet =
    let
        textNode =
            magnet.text
                |> Text.fromString
                |> Text.color Color.white
                |> rendered

        bgNode =
            rectangle (Layout.width textNode + 10) (Layout.height textNode + 10)
                |> filled (uniform Color.black)
    in
        group
            [ textNode, bgNode ]
            |> shift magnet.position
