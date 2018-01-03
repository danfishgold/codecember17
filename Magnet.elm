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
    { width : Float
    , height : Float
    }


type alias Edges =
    { minX : Float
    , maxX : Float
    , minY : Float
    , maxY : Float
    }


type Data a b
    = Atom a
    | Composite b (List a)


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


edges : Magnet a -> Edges
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


stopDragging : Magnets a -> Magnets a
stopDragging magnets =
    case magnets.dragging of
        Nothing ->
            magnets

        Just m ->
            { magnets
                | stationary = mergeOrAdd simpleJoiner m magnets.stationary
                , dragging = Nothing
            }


mergeOrAdd : (Magnet a -> Magnet a -> Magnet a) -> Magnet a -> List (Magnet a) -> List (Magnet a)
mergeOrAdd joiner magnet magnets =
    magnets
        |> List.map (\m -> ( m, edges m ))
        |> recurseMergeOrAdd joiner magnet


adjecentInX : Edges -> Edges -> Bool
adjecentInX a b =
    abs (a.minX - b.maxX) <= 30 || abs (b.minX - a.maxX) <= 30


adjecentInY : Edges -> Edges -> Bool
adjecentInY a b =
    a.minY <= b.maxY && a.maxY >= b.minY


adjecent : Edges -> Edges -> Bool
adjecent a b =
    adjecentInX a b && adjecentInY a b


recurseMergeOrAdd : (Magnet a -> Magnet a -> Magnet a) -> Magnet a -> List ( Magnet a, Edges ) -> List (Magnet a)
recurseMergeOrAdd joiner magnet magnetsWithEdges =
    let
        magnetEdges =
            edges magnet
    in
        case filterFirst (\( _, currentEdges ) -> adjecent currentEdges magnetEdges) magnetsWithEdges of
            ( _, Nothing ) ->
                magnet :: List.map Tuple.first magnetsWithEdges

            ( others, Just ( closeMagnet, _ ) ) ->
                recurseMergeOrAdd joiner (joiner magnet closeMagnet) others


simpleJoiner : Magnet a -> Magnet a -> Magnet a
simpleJoiner a b =
    let
        aEdges =
            edges a

        bEdges =
            edges b

        position =
            ( (min aEdges.minX bEdges.minX + max aEdges.maxX bEdges.maxX) / 2
            , (min aEdges.minY bEdges.minY + max aEdges.maxY bEdges.maxY) / 2
            )

        padding =
            { width = max a.padding.width b.padding.width
            , height = max a.padding.height b.padding.height
            }

        ( left, right ) =
            if Tuple.first a.position < Tuple.first b.position then
                ( a, b )
            else
                ( b, a )

        textOrSpace text =
            if text == "_" then
                " "
            else
                text
    in
        { data = a.data
        , text = textOrSpace left.text ++ textOrSpace right.text
        , position = position
        , padding = padding
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
                    ( List.reverse falses, Nothing )

                head :: rest ->
                    if fn head then
                        ( List.reverse falses ++ rest, Just head )
                    else
                        recurse fn rest (head :: falses)
    in
        recurse fn xs []
