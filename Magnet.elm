module Magnet exposing (..)

import Collage exposing (..)
import Collage.Text as Text
import Collage.Layout as Layout
import Color exposing (Color)
import Point
import Pointer exposing (Pointer)
import Pointer.Mapping exposing (Mapping)


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
    , padding = defaultPadding
    }


defaultPadding : Size
defaultPadding =
    { width = 15, height = 15 }


element : Color -> Magnet a -> Collage msg
element color { text, position, padding } =
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
    , dragging : Mapping (Magnet a)
    , sources : List (Magnet a)
    }


magnetsView : (Magnet a -> Color) -> Magnets a -> Collage msg
magnetsView color { stationary, dragging, sources } =
    List.concat
        [ Pointer.Mapping.toList dragging
        , stationary
        , sources
        ]
        |> List.map (\m -> element (color m) m)
        |> group


startDragging : Mapping Pointer -> Magnets a -> Magnets a
startDragging pointers magnets =
    Pointer.Mapping.foldl maybePickUp magnets pointers


keepDragging : Mapping Pointer -> Mapping Pointer -> Magnets a -> Magnets a
keepDragging oldPointers newPointers magnets =
    { magnets
        | dragging =
            Pointer.Mapping.mutualMap3
                (\oldP newP m ->
                    moveBy (Point.sub newP.position oldP.position) m
                )
                oldPointers
                newPointers
                magnets.dragging
    }


stopDragging : Mapping Pointer -> Magnets a -> Magnets a
stopDragging pointers magnets =
    let
        ( stillDragging, stoppedDragging ) =
            Pointer.Mapping.extract (Pointer.Mapping.ids pointers) magnets.dragging
    in
        { magnets
            | stationary =
                stoppedDragging
                    |> List.foldl (mergeOrAdd simpleJoiner) magnets.stationary
            , dragging = stillDragging
        }


maybePickUp : Pointer.Id -> Pointer -> Magnets a -> Magnets a
maybePickUp identifier pointer magnets =
    let
        ( newStationary, draggingFromStationary ) =
            filterFirst (contains pointer.position) magnets.stationary

        dragging =
            if draggingFromStationary == Nothing then
                filterFirst (contains pointer.position) magnets.sources
                    |> Tuple.second
            else
                draggingFromStationary
    in
        case dragging of
            Nothing ->
                magnets

            Just m ->
                let
                    pickedUp =
                        case pointer.source of
                            Pointer.Mouse ->
                                m

                            Pointer.Touch ->
                                moveBy ( 0, 25 ) m
                in
                    { magnets
                        | stationary = newStationary
                        , dragging =
                            Pointer.Mapping.add identifier
                                pickedUp
                                magnets.dragging
                    }


mergeOrAdd : (Magnet a -> Magnet a -> Magnet a) -> Magnet a -> List (Magnet a) -> List (Magnet a)
mergeOrAdd joiner magnet magnets =
    let
        recurse joiner magnet magnetsWithEdges =
            let
                magnetEdges =
                    edges magnet

                isAdjacent ( _, currentEdges ) =
                    adjecent currentEdges magnetEdges
            in
                case filterFirst isAdjacent magnetsWithEdges of
                    ( _, Nothing ) ->
                        magnet :: List.map Tuple.first magnetsWithEdges

                    ( others, Just ( closeMagnet, _ ) ) ->
                        recurse joiner (joiner magnet closeMagnet) others
    in
        magnets
            |> List.map (\m -> ( m, edges m ))
            |> recurse joiner magnet


adjecentInX : Edges -> Edges -> Bool
adjecentInX a b =
    abs (a.minX - b.maxX) <= 30 || abs (b.minX - a.maxX) <= 30


adjecentInY : Edges -> Edges -> Bool
adjecentInY a b =
    a.minY <= b.maxY && a.maxY >= b.minY


adjecent : Edges -> Edges -> Bool
adjecent a b =
    adjecentInX a b && adjecentInY a b


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
            if text == "[space]" then
                " "
            else
                text
    in
        { data = a.data
        , text = textOrSpace left.text ++ textOrSpace right.text
        , position = position
        , padding = padding
        }


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


organizeSources : Size -> Size -> List (Magnet a) -> List (Magnet a)
organizeSources area padding sources =
    let
        sized =
            List.map (\m -> ( m, size m )) sources

        rowHeight =
            sized
                |> List.map (\( _, { height } ) -> height)
                |> List.maximum
                |> Maybe.withDefault 0
                |> (+) padding.height

        moveTopLeftTo ( x0, y0 ) ( magnet, size ) =
            { magnet
                | position =
                    ( x0 + size.width / 2 - area.width / 2
                    , area.height / 2 - (y0 + size.height / 2)
                    )
            }

        shouldPlaceHere currentX magnet size =
            -- can fit in current row
            (currentX + 2 * padding.width + size.width <= area.width)
                -- or (can or can't fit && at beginning of row)
                ||
                    (currentX == 0)

        recurse ( currentX, currentY ) finished remaining =
            case remaining of
                [] ->
                    finished

                ( magnet, size ) :: rest ->
                    if shouldPlaceHere currentX magnet size then
                        recurse ( currentX + padding.width + size.width, currentY )
                            (moveTopLeftTo ( currentX + padding.width, currentY )
                                ( magnet, size )
                                :: finished
                            )
                            rest
                    else
                        recurse
                            ( 0, currentY + rowHeight )
                            finished
                            remaining
    in
        List.reverse <| recurse ( 0, padding.height ) [] sized


reorderSources : Size -> Size -> Magnets a -> Magnets a
reorderSources area padding magnets =
    { magnets | sources = organizeSources area padding magnets.sources }
