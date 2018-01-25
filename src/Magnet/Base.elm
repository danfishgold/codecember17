module Magnet.Base
    exposing
        ( Data
        , Magnet
        , magnet
        , data
        , addPadding
        , setAlpha
        , setHighlight
        , setBackground
        , element
        )

import Collage exposing (Collage)
import Color exposing (Color)
import Point
import Util exposing (Size)
import TextRect


type alias Point =
    Point.Point


type alias Data a =
    { a
        | background : Color
        , textColor : Color
    }


type alias Magnet data =
    { data : Data data
    , text : String
    , position : Point
    , padding : Size
    , highlighted : Maybe Color
    }


magnet : String -> Data data -> Magnet data
magnet text data =
    { data = data
    , text = text
    , position = ( 0, 0 )
    , padding = TextRect.defaultPadding
    , highlighted = Nothing
    }


data : Color -> Color -> Data {}
data background textColor =
    { background = background
    , textColor = textColor
    }


addPadding : Float -> Magnet data -> Magnet data
addPadding delta magnet =
    { magnet
        | padding =
            { width = magnet.padding.width + delta
            , height = magnet.padding.height + delta
            }
    }


setAlpha : Float -> Color -> Color
setAlpha alpha c =
    let
        { red, green, blue } =
            Color.toRgb c
    in
        Color.rgba red green blue alpha


setHighlight : Maybe Color -> Magnet data -> Magnet data
setHighlight highlighted magnet =
    { magnet | highlighted = highlighted }


setBackground : Color -> Magnet data -> Magnet data
setBackground bg magnet =
    let
        data =
            magnet.data
    in
        { magnet | data = { data | background = bg } }


element : Bool -> Magnet data -> Collage msg
element isDragging magnet =
    let
        bg =
            magnet.highlighted |> Maybe.withDefault magnet.data.background
    in
        if isDragging then
            TextRect.view (setAlpha 0.8 bg) magnet.data.textColor (addPadding 5 magnet)
        else
            TextRect.view bg magnet.data.textColor magnet
