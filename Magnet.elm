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
import Point
import Pointer exposing (Pointer)
import Pointer.Mapping exposing (Mapping)
import Util exposing (Size, filterFirst)
import TextRect exposing (contains, moveBy)
import Magnet.Base as Base exposing (Magnet, setHighlight)
import Magnet.Category as Category exposing (Category)
import Magnet.Interaction as Interaction exposing (Interaction)
import Color exposing (Color)


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


keepDragging : Interaction data -> (Magnet data -> Color) -> Mapping Pointer -> Mapping Pointer -> Magnets data -> Magnets data
keepDragging interaction defaultBackground oldPointers newPointers magnets =
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
                    (\_ m ->
                        Interaction.hover interaction
                            defaultBackground
                            ( magnets.stationary, magnets.sources )
                            m
                    )
    }


stopDragging : Interaction data -> Mapping Pointer -> Magnets data -> Magnets data
stopDragging interaction pointers magnets =
    let
        ( stillDragging, stoppedDragging ) =
            Pointer.Mapping.extract (Pointer.Mapping.ids pointers) magnets.dragging

        ( newStationary, newSources ) =
            stoppedDragging
                |> List.map (setHighlight Nothing)
                |> List.foldl (Interaction.interactOrAdd interaction)
                    ( magnets.stationary, magnets.sources )
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
