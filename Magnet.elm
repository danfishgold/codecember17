module Magnet
    exposing
        ( Magnets
        , category
        , repositionSources
        , magnetsView
        , startDragging
        , keepDragging
        , stopDragging
        )

import Collage exposing (Collage, group)
import Color exposing (Color)
import Point
import Pointer exposing (Pointer)
import Pointer.Mapping exposing (Mapping)
import Types exposing (Size, Edges)
import TextRect exposing (edges, contains, moveBy)


type alias Point =
    Point.Point


type alias Magnet a =
    { data : a
    , text : String
    , position : Point
    , padding : Size
    , highlighted : Bool
    }


magnet : String -> a -> Magnet a
magnet text data =
    { data = data
    , text = text
    , position = ( 0, 0 )
    , padding = TextRect.defaultPadding
    , highlighted = False
    }


addPadding : Float -> Magnet a -> Magnet a
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


element : Color -> Magnet a -> Bool -> Collage msg
element background magnet isDragging =
    let
        bg =
            if magnet.highlighted then
                Color.red
            else
                background
    in
        if isDragging then
            TextRect.view (setAlpha 0.8 bg) Color.white (addPadding 5 magnet)
        else
            TextRect.view bg Color.white magnet



--


type alias Category a =
    { name : String
    , sources : List (Magnet a)
    }


type alias Magnets a =
    { stationary : List (Magnet a)
    , dragging : Mapping (Magnet a)
    , sources : List (Category a)
    }


category : String -> List String -> Category Color
category name strings =
    strings
        |> List.map (flip magnet Color.black)
        |> \sources -> { name = name, sources = sources }


allSources : List (Category a) -> List (Magnet a)
allSources categories =
    List.concatMap .sources categories


magnetsView : (Magnet a -> Color) -> Magnets a -> Collage msg
magnetsView color { stationary, dragging, sources } =
    List.concat
        [ Pointer.Mapping.toList dragging
            |> List.map (\m -> element (color m) m True)
        , stationary
            ++ allSources sources
            |> List.map (\m -> element (color m) m False)
        ]
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
                |> Pointer.Mapping.map (\id m -> highlightNear magnets.stationary m)
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
                    |> List.map (setHighlight False)
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
                filterFirst (contains pointer.position) (allSources magnets.sources)
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


mergeOrAdd : (Magnet a -> Magnet a -> Maybe (List (Magnet a))) -> Magnet a -> List (Magnet a) -> List (Magnet a)
mergeOrAdd joiner droppedMagnet magnets =
    let
        isAdjacent magnet =
            adjecent (edges magnet) (edges droppedMagnet)
    in
        case filterFirst isAdjacent magnets of
            ( _, Nothing ) ->
                droppedMagnet :: magnets

            ( others, Just closeMagnet ) ->
                case joiner droppedMagnet closeMagnet of
                    Nothing ->
                        droppedMagnet :: magnets

                    Just newMagnets ->
                        newMagnets ++ others


adjecentInX : Edges -> Edges -> Bool
adjecentInX a b =
    abs (a.minX - b.maxX) <= 30 || abs (b.minX - a.maxX) <= 30


adjecentInY : Edges -> Edges -> Bool
adjecentInY a b =
    a.minY <= b.maxY && a.maxY >= b.minY


adjecent : Edges -> Edges -> Bool
adjecent a b =
    adjecentInX a b && adjecentInY a b


simpleJoiner : Magnet a -> Magnet a -> Maybe (List (Magnet a))
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
        Just
            [ { data = a.data
              , text = textOrSpace left.text ++ textOrSpace right.text
              , position = position
              , padding = padding
              , highlighted = False
              }
            ]


highlightNear : List (Magnet a) -> Magnet a -> Magnet a
highlightNear stationary dragging =
    let
        draggingEdges =
            edges dragging
    in
        stationary
            |> List.any (edges >> adjecent draggingEdges)
            |> flip setHighlight dragging


setHighlight : Bool -> Magnet a -> Magnet a
setHighlight highlighted magnet =
    { magnet | highlighted = highlighted }


filterFirst : (a -> Bool) -> List a -> ( List a, Maybe a )
filterFirst fn xs =
    let
        recurse lst falses =
            case lst of
                [] ->
                    ( List.reverse falses, Nothing )

                head :: rest ->
                    if fn head then
                        ( List.reverse falses ++ rest, Just head )
                    else
                        recurse rest (head :: falses)
    in
        recurse xs []


repositionSources : Size -> Size -> Magnets a -> Magnets a
repositionSources area padding magnets =
    let
        folder category ( previous, currentY ) =
            let
                ( organized, nextY ) =
                    TextRect.organizeInRows currentY area padding category.sources
            in
                ( { category | sources = organized } :: previous, nextY )
    in
        { magnets
            | sources =
                List.foldl folder ( [], 0 ) magnets.sources
                    |> Tuple.first
                    |> List.reverse
        }
