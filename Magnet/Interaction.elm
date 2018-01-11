module Magnet.Interaction exposing (..)

import Magnet.Base exposing (Magnet)
import Magnet.Category as Category exposing (Category)
import TextRect exposing (edges)
import Util exposing (Edges, filterFirst, maybeOr, between)


{-|
Interactor droppedMagnet other isTheOtherASource =
    Just (magnets to add, sources to add)
   or
    Nothing -- the interaction failed
-}
type alias Interactor data =
    Magnet data -> Magnet data -> Bool -> Maybe ( List (Magnet data), List (Category data) )


{-|
I want to highlight magnets which can interact, so instead of calculating the result
each time while the dragged magnet is still dragging, I can just check whether
there's an interactor function at all
-}
type alias Interaction data =
    Magnet data -> Magnet data -> Bool -> Maybe (Interactor data)


apply : Interaction data -> Interactor data
apply interaction a b isSource =
    case interaction a b isSource of
        Nothing ->
            Nothing

        Just interactor ->
            interactor a b isSource


willInteract : Interaction data -> Magnet data -> Magnet data -> Bool -> Bool
willInteract interaction a b isSource =
    interaction a b isSource /= Nothing


interactOrAdd :
    Interaction data
    -> Magnet data
    -> ( List (Magnet data), List (Category data) )
    -> ( List (Magnet data), List (Category data) )
interactOrAdd interaction droppedMagnet ( magnets, sources ) =
    Nothing
        |> maybeOr (\_ -> interactWithMagnets interaction droppedMagnet ( magnets, sources ))
        |> maybeOr (\_ -> interactWithSources interaction droppedMagnet ( magnets, sources ))
        |> Maybe.withDefault ( droppedMagnet :: magnets, sources )


interactWithMagnets :
    Interaction data
    -> Magnet data
    -> ( List (Magnet data), List (Category data) )
    -> Maybe ( List (Magnet data), List (Category data) )
interactWithMagnets interaction droppedMagnet ( magnets, sources ) =
    let
        isNear magnet =
            near (edges magnet) (edges droppedMagnet)
    in
        case filterFirst isNear magnets of
            ( _, Nothing ) ->
                Nothing

            ( others, Just closeMagnet ) ->
                apply interaction droppedMagnet closeMagnet False
                    |> Maybe.map
                        (\( newMagnets, newSources ) ->
                            ( newMagnets ++ others, Category.merge newSources sources )
                        )


interactWithSources :
    Interaction data
    -> Magnet data
    -> ( List (Magnet data), List (Category data) )
    -> Maybe ( List (Magnet data), List (Category data) )
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
                    apply interaction droppedMagnet closeSource True
                        |> Maybe.map
                            (\( newMagnets, newSources ) ->
                                ( newMagnets ++ magnets, Category.merge newSources others )
                            )
    else
        Nothing


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


simple : Interaction data
simple a b isSource =
    Just simpleInteractor


simpleInteractor : Interactor data
simpleInteractor a b isSource =
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
