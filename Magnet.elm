module Magnet exposing (..)

import Collage exposing (..)
import Collage.Text as Text
import Collage.Layout as Layout
import Color exposing (Color)
import Point
import Pointer


type alias Point =
    Point.Point


type alias Size =
    { width : Float, height : Float }


type alias Magnet a =
    { data : a
    , text : String
    , position : Point
    , padding : Size
    }


magnet : String -> Point -> a -> Magnet a
magnet text position data =
    { data = data
    , text = text
    , position = position
    , padding = { width = 15, height = 15 }
    }


view : Color -> Magnet a -> Collage msg
view color { text, position, padding } =
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
                |> filled (uniform color)
    in
        group
            [ textNode, bgNode ]
            |> shift position


size : Magnet a -> Size
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



--


type alias Magnets a =
    { stationary : List (Magnet a)
    , dragging : Maybe (Magnet a)
    , sources : List (Magnet a)
    }


magnetsView : (Magnet a -> Color) -> Magnets a -> Collage msg
magnetsView color { stationary, dragging, sources } =
    List.concat
        [ Maybe.map List.singleton dragging |> Maybe.withDefault []
        , stationary
        , sources
        ]
        |> List.map (\m -> view (color m) m)
        |> group


stopDragging : Magnets a -> Magnets a
stopDragging magnets =
    case magnets.dragging of
        Nothing ->
            magnets

        Just m ->
            { magnets
                | stationary = m :: magnets.stationary
                , dragging = Nothing
            }


startDragging : Point -> Magnets a -> Magnets a
startDragging pointer magnets =
    let
        ( newStationary, draggingFromStationary ) =
            filterFirst (contains pointer) magnets.stationary

        dragging =
            if draggingFromStationary == Nothing then
                filterFirst (contains pointer) magnets.sources
                    |> Tuple.second
            else
                draggingFromStationary
    in
        { magnets | stationary = newStationary, dragging = dragging }


keepDragging : Point -> Point -> Magnets a -> Magnets a
keepDragging oldPointer newPointer magnets =
    case magnets.dragging of
        Nothing ->
            magnets

        Just m ->
            { magnets
                | dragging =
                    moveBy (Point.sub newPointer oldPointer) m
                        |> Just
            }


drag : Point -> Pointer.Event -> Magnets a -> Magnets a
drag oldPointer event magnets =
    case event.state of
        Pointer.Start ->
            startDragging event.pointer magnets

        Pointer.Move ->
            keepDragging oldPointer event.pointer magnets

        Pointer.End ->
            stopDragging magnets

        Pointer.Cancel ->
            stopDragging magnets


filterFirst : (a -> Bool) -> List a -> ( List a, Maybe a )
filterFirst fn xs =
    let
        recurse fn xs falses =
            case xs of
                [] ->
                    ( falses, Nothing )

                head :: rest ->
                    if fn head then
                        ( rest ++ falses, Just head )
                    else
                        recurse fn rest (head :: falses)
    in
        recurse fn xs []
