module Magnet.Interaction
    exposing
        ( Interaction
        , horizontal
        , willInteract
        , interactOrAdd
        , RelativePosition(..)
        , relativePosition
        )

import Magnet.Base exposing (Magnet)
import Magnet.Category as Category exposing (Category)
import TextRect exposing (edges)
import Util exposing (Edges, filterFirst, maybeOr, between)


{-|
Interactor isTheOtherASource droppedMagnet other =
    Just (magnets to add, sources to add)
   or
    Nothing -- the interaction failed
-}
type alias Interactor data =
    Bool -> Magnet data -> Magnet data -> Maybe ( List (Magnet data), List (Category data) )


{-|
I want to highlight magnets which can interact, so instead of calculating the result
each time while the dragged magnet is still dragging, I can just check whether
there's an interactor function at all
-}
type alias Interaction data =
    Bool -> Magnet data -> Magnet data -> Maybe (Interactor data)


apply : Interaction data -> Interactor data
apply interaction isSource a b =
    case interaction isSource a b of
        Nothing ->
            Nothing

        Just interactor ->
            interactor isSource a b


willInteract : Interaction data -> Bool -> Magnet data -> Magnet data -> Bool
willInteract interaction isSource a b =
    interaction isSource a b /= Nothing


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
                apply interaction False droppedMagnet closeMagnet
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
    let
        isNear source =
            near (edges source) (edges droppedMagnet)
    in
        case Category.filterFirst isNear sources of
            ( _, Nothing ) ->
                Nothing

            ( others, Just closeSource ) ->
                apply interaction True droppedMagnet closeSource
                    |> Maybe.map
                        (\( newMagnets, newSources ) ->
                            ( newMagnets ++ magnets, Category.merge newSources others )
                        )


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
        if near aEdges bEdges then
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
        else
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


horizontal : Interaction data
horizontal isSource a b =
    if isSource then
        Nothing
    else
        case relativePosition a b of
            Just Left ->
                Just simpleInteractor

            Just Right ->
                Just simpleInteractor

            _ ->
                Nothing


simpleInteractor : Interactor data
simpleInteractor isSource a b =
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
