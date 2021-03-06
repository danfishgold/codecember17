module Magnet exposing
    ( Environment
    , Magnets
    , initial
    , keepDragging
    , magnetsView
    , repositionSources
    , startDragging
    , stopDragging
    )

import Collage exposing (Collage, group)
import Magnet.Base as Base exposing (Magnet, setHighlight)
import Magnet.Category as Category exposing (Category)
import Magnet.Interaction as Interaction exposing (Interaction)
import Point
import Pointer exposing (Pointer)
import Pointer.Mapping exposing (Mapping)
import TextRect exposing (contains, moveBy)
import Util exposing (Direction(..), Size, filterFirst)


type alias Magnets data =
    { stationary : List (Magnet data)
    , dragging : Mapping (Magnet data)
    , sources : List (Category data)
    }


type alias Environment data =
    { sources : List (Category data)
    , interaction : Interaction data
    , sourcesDirection : Direction
    }


initial : List (Category data) -> Magnets data
initial sources =
    { stationary = []
    , dragging = Pointer.Mapping.empty
    , sources = sources
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


keepDragging : Interaction data -> Mapping Pointer -> Mapping Pointer -> Magnets data -> Magnets data
keepDragging interaction oldPointers newPointers magnets =
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
                            ( magnets.stationary, magnets.sources )
                            m
                    )
    }


stopDragging : Interaction data -> Mapping Pointer -> Magnets data -> Magnets data
stopDragging interaction pointers magnets =
    let
        ( stillDragging, stoppedDragging ) =
            Pointer.Mapping.extract (Pointer.Mapping.ids pointers) magnets.dragging
                |> Tuple.mapSecond (List.map (\m -> Base.setHighlight Nothing m))

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


repositionSources : Direction -> Size -> Size -> Magnets data -> Magnets data
repositionSources dir area padding magnets =
    let
        folder category ( previous, currentY ) =
            let
                ( organized, nextY ) =
                    TextRect.organizeInRows dir currentY area padding category.sources
            in
            ( { category | sources = organized } :: previous, nextY )
    in
    { magnets
        | sources =
            List.foldl folder ( [], 0 ) magnets.sources
                |> Tuple.first
                |> List.reverse
    }
