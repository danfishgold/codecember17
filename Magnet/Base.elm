module Magnet.Base exposing (..)

import Collage exposing (Collage, group)
import Color exposing (Color)
import Point
import Util exposing (Size, Edges, filterFirst, between)
import TextRect exposing (edges, contains, moveBy)


type alias Point =
    Point.Point


type alias Data a =
    { a
        | background : Color
        , textColor : Color
        , interactsWithSources : Bool
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
    , interactsWithSources = False
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


near : Edges -> Edges -> Bool
near a b =
    let
        betweenX =
            between (b.minX - 30) (b.maxX + 30)

        betweenY =
            between (b.minY - 30) (b.maxY + 30)
    in
        (betweenX a.minX || betweenX a.maxX)
            && (betweenY a.minY || betweenY a.maxY)
