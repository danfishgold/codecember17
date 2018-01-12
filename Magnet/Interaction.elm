module Magnet.Interaction
    exposing
        ( Interaction
        , horizontal
        , willInteract
        , interactOrAdd
        )

import Magnet.Base exposing (Magnet)
import Magnet.Category as Category exposing (Category)
import RelativePosition exposing (RelativePosition(..), relativePosition, keepEdgeInPlace)
import Util exposing (Edges, filterFirst, filterMapFirst, maybeOr, between)


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
    case filterMapFirst (interaction False droppedMagnet) magnets of
        ( _, Nothing ) ->
            Nothing

        ( others, Just ( closeMagnet, interactor ) ) ->
            interactor False droppedMagnet closeMagnet
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
    case Category.filterMapFirst (interaction True droppedMagnet) sources of
        ( _, Nothing ) ->
            Nothing

        ( others, Just ( closeSource, interactor ) ) ->
            interactor True droppedMagnet closeSource
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
                Just (simpleInteractor Left)

            Just Right ->
                Just (simpleInteractor Right)

            _ ->
                Nothing


simpleInteractor : RelativePosition -> Interactor data
simpleInteractor rPos isSource a b =
    let
        padding =
            { width = max a.padding.width b.padding.width
            , height = max a.padding.height b.padding.height
            }

        ( left, right ) =
            if rPos == Left then
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
                , position = ( 0, 0 )
                , padding = padding
                , highlighted = Nothing
                }
                    |> keepEdgeInPlace (RelativePosition.opposite rPos) b
              ]
            , []
            )
