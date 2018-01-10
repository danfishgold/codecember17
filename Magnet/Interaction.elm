module Magnet.Interaction exposing (..)

import Magnet.Base exposing (Magnet, filterFirst, near)
import Magnet.Category as Category exposing (Category)
import TextRect exposing (edges)


{-|
Interaction droppedMagnet other isTheOtherASource =
    Just (magnets to add, sources to add)
   or
    Nothing -- the interaction failed

-}
type alias Interaction data =
    Magnet data -> Magnet data -> Bool -> Maybe ( List (Magnet data), List (Category data) )


simple : Interaction data
simple a b isSource =
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
            ( [ { data = a.data
                , text = textOrSpace left.text ++ textOrSpace right.text
                , position = position
                , padding = padding
                , highlighted = Nothing
                }
              ]
            , []
            )


interactOrAdd : Interaction data -> Magnet data -> ( List (Magnet data), List (Category data) ) -> ( List (Magnet data), List (Category data) )
interactOrAdd interaction droppedMagnet ( magnets, sources ) =
    Nothing
        |> maybeOr (\_ -> interactWithMagnets interaction droppedMagnet ( magnets, sources ))
        |> maybeOr (\_ -> interactWithSources interaction droppedMagnet ( magnets, sources ))
        |> Maybe.withDefault ( droppedMagnet :: magnets, sources )


interactWithMagnets : Interaction data -> Magnet data -> ( List (Magnet data), List (Category data) ) -> Maybe ( List (Magnet data), List (Category data) )
interactWithMagnets interaction droppedMagnet ( magnets, sources ) =
    let
        isNear magnet =
            near (edges magnet) (edges droppedMagnet)
    in
        case filterFirst isNear magnets of
            ( _, Nothing ) ->
                Nothing

            ( others, Just closeMagnet ) ->
                interaction droppedMagnet closeMagnet False
                    |> Maybe.map
                        (\( newMagnets, newSources ) ->
                            ( newMagnets ++ others, Category.merge newSources sources )
                        )


interactWithSources : Interaction data -> Magnet data -> ( List (Magnet data), List (Category data) ) -> Maybe ( List (Magnet data), List (Category data) )
interactWithSources interaction droppedMagnet ( magnets, sources ) =
    if droppedMagnet.data.interactsWithSources then
        let
            isNear source =
                near (edges source) (edges droppedMagnet)
        in
            case Category.filterFirst isNear sources of
                ( _, Nothing ) ->
                    Nothing

                ( others, Just closeSource ) ->
                    interaction droppedMagnet closeSource True
                        |> Maybe.map
                            (\( newMagnets, newSources ) ->
                                ( newMagnets ++ magnets, Category.merge newSources others )
                            )
    else
        Nothing


maybeOr : (() -> Maybe a) -> Maybe a -> Maybe a
maybeOr lazyOther current =
    if current == Nothing then
        lazyOther ()
    else
        current
