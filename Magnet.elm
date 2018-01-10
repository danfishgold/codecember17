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


type alias Magnet data =
    { data : data
    , text : String
    , position : Point
    , padding : Size
    , highlighted : Maybe Color
    }


magnet : String -> data -> Magnet data
magnet text data =
    { data = data
    , text = text
    , position = ( 0, 0 )
    , padding = TextRect.defaultPadding
    , highlighted = Nothing
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


element : Color -> Magnet data -> Bool -> Collage msg
element background magnet isDragging =
    let
        bg =
            magnet.highlighted |> Maybe.withDefault background
    in
        if isDragging then
            TextRect.view (setAlpha 0.8 bg) Color.white (addPadding 5 magnet)
        else
            TextRect.view bg Color.white magnet



--


type alias Category data =
    { name : String
    , sources : List (Magnet data)
    }


type alias Magnets data =
    { stationary : List (Magnet data)
    , dragging : Mapping (Magnet data)
    , sources : List (Category data)
    }


category : String -> List String -> Category Color
category name strings =
    strings
        |> List.map (flip magnet Color.black)
        |> \sources -> { name = name, sources = sources }


allSources : List (Category data) -> List (Magnet data)
allSources categories =
    List.concatMap .sources categories


magnetsView : (Magnet data -> Color) -> Magnets data -> Collage msg
magnetsView color { stationary, dragging, sources } =
    List.concat
        [ Pointer.Mapping.toList dragging
            |> List.map (\m -> element (color m) m True)
        , stationary
            ++ allSources sources
            |> List.map (\m -> element (color m) m False)
        ]
        |> group


startDragging : Mapping Pointer -> Magnets data -> Magnets data
startDragging pointers magnets =
    Pointer.Mapping.foldl maybePickUp magnets pointers


keepDragging : Mapping Pointer -> Mapping Pointer -> Magnets data -> Magnets data
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


stopDragging : Mapping Pointer -> Magnets data -> Magnets data
stopDragging pointers magnets =
    let
        ( stillDragging, stoppedDragging ) =
            Pointer.Mapping.extract (Pointer.Mapping.ids pointers) magnets.dragging
    in
        { magnets
            | stationary =
                stoppedDragging
                    |> List.map (setHighlight Nothing)
                    |> List.foldl (mergeOrAdd simpleJoiner) magnets.stationary
            , dragging = stillDragging
        }


maybePickUp : Pointer.Id -> Pointer -> Magnets data -> Magnets data
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


mergeOrAdd : (Magnet data -> Magnet data -> Maybe (List (Magnet data))) -> Magnet data -> List (Magnet data) -> List (Magnet data)
mergeOrAdd joiner droppedMagnet magnets =
    let
        isNear magnet =
            near (edges magnet) (edges droppedMagnet)
    in
        case filterFirst isNear magnets of
            ( _, Nothing ) ->
                droppedMagnet :: magnets

            ( others, Just closeMagnet ) ->
                case joiner droppedMagnet closeMagnet of
                    Nothing ->
                        droppedMagnet :: magnets

                    Just newMagnets ->
                        newMagnets ++ others


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


between : Float -> Float -> Float -> Bool
between a b x =
    a <= x && x <= b


ordered : Float -> Float -> Float -> Bool
ordered a b c =
    between a c b


simpleJoiner : Magnet data -> Magnet data -> Maybe (List (Magnet data))
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
              , highlighted = Nothing
              }
            ]


highlightNear : List (Magnet data) -> Magnet data -> Magnet data
highlightNear stationary dragging =
    let
        draggingEdges =
            edges dragging
    in
        stationary
            |> filterFirst (edges >> near draggingEdges)
            |> Tuple.second
            |> Maybe.map (relativePosition dragging >> highlightColor)
            |> flip setHighlight dragging


setHighlight : Maybe Color -> Magnet data -> Magnet data
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


repositionSources : Size -> Size -> Magnets data -> Magnets data
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


type RelativePosition
    = Left
    | Right
    | Up
    | Down
    | On


{-| The position of `a` relative to `b`
-}
relativePosition : Magnet a -> Magnet a -> Maybe RelativePosition
relativePosition a b =
    let
        ( aEdges, bEdges ) =
            ( edges a, edges b )

        ( ( aX, aY ), ( bX, bY ) ) =
            ( a.position, b.position )

        ( insideX, insideY ) =
            ( aX |> between bEdges.minX bEdges.maxX
            , aY |> between bEdges.minY bEdges.maxY
            )
    in
        case ( insideX, insideY ) of
            ( True, True ) ->
                Just On

            ( True, False ) ->
                if aY > bY then
                    Just Up
                else
                    Just Down

            ( False, True ) ->
                if aX > bX then
                    Just Right
                else
                    Just Left

            ( False, False ) ->
                Nothing


highlightColor : Maybe RelativePosition -> Color
highlightColor pos =
    case pos of
        Nothing ->
            Color.black

        Just Left ->
            Color.darkRed

        Just Right ->
            Color.lightRed

        Just Up ->
            Color.darkBlue

        Just Down ->
            Color.lightBlue

        Just On ->
            Color.darkGreen
