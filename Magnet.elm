module Magnet
    exposing
        ( Magnets
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
import Util exposing (Size, Edges, filterFirst, maybeOr)
import TextRect exposing (edges, contains, moveBy)
import Magnet.Base as Base exposing (Magnet, setHighlight)
import Magnet.Category as Category exposing (Category)
import Magnet.Interaction as Interaction
    exposing
        ( Interaction
        , RelativePosition(..)
        )


type alias Magnets data =
    { stationary : List (Magnet data)
    , dragging : Mapping (Magnet data)
    , sources : List (Category data)
    }


magnetsView : Magnets data -> Collage msg
magnetsView { stationary, dragging, sources } =
    List.concat
        [ Pointer.Mapping.toList dragging
            |> List.map (Base.element True)
        , stationary
            ++ Category.allSources sources
            |> List.map (Base.element False)
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
                |> Pointer.Mapping.map
                    (\id m ->
                        highlightNear Interaction.horizontal
                            magnets.stationary
                            magnets.sources
                            m
                    )
    }


stopDragging : Mapping Pointer -> Magnets data -> Magnets data
stopDragging pointers magnets =
    let
        ( stillDragging, stoppedDragging ) =
            Pointer.Mapping.extract (Pointer.Mapping.ids pointers) magnets.dragging

        ( newStationary, newSources ) =
            stoppedDragging
                |> List.map (setHighlight Nothing)
                |> List.foldl (Interaction.interactOrAdd Interaction.horizontal) ( magnets.stationary, magnets.sources )
    in
        { stationary = newStationary
        , sources = newSources
        , dragging = stillDragging
        }


maybePickUp : Pointer.Id -> Pointer -> Magnets data -> Magnets data
maybePickUp identifier pointer magnets =
    let
        ( newStationary, draggingFromStationary ) =
            filterFirst (contains pointer.position) magnets.stationary

        dragging =
            if draggingFromStationary == Nothing then
                filterFirst (contains pointer.position) (Category.allSources magnets.sources)
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


highlightNear : Interaction data -> List (Magnet data) -> List (Category data) -> Magnet data -> Magnet data
highlightNear interaction stationary sources dragging =
    let
        stationaryInteraction () =
            stationary
                |> filterFirst (Interaction.willInteract interaction False dragging)
                |> Tuple.second

        sourceInteraction () =
            if dragging.data.interactsWithSources then
                sources
                    |> Category.allSources
                    |> filterFirst (Interaction.willInteract interaction True dragging)
                    |> Tuple.second
            else
                Nothing
    in
        Nothing
            |> maybeOr stationaryInteraction
            |> maybeOr sourceInteraction
            |> Maybe.map (Interaction.relativePosition dragging >> highlightColor)
            |> flip setHighlight dragging


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
