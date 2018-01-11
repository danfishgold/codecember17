module Magnet.Interaction
    exposing
        ( Interaction
        , horizontal
        , willInteract
        , interactOrAdd
        )

import Magnet.Base exposing (Magnet)
import Magnet.Category as Category exposing (Category)
import TextRect exposing (edges, RelativePosition(..), relativePosition)
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
            relativePosition magnet droppedMagnet /= Nothing
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
            relativePosition source droppedMagnet /= Nothing
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
        position =
            TextRect.listCenter [ a, b ]

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
