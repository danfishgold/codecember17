module RelativePosition
    exposing
        ( RelativePosition(..)
        , relativePosition
        , opposite
        , keepEdgeInPlace
        )

import Util exposing (Edges, between)
import TextRect exposing (TextRect, edges, size)


type RelativePosition
    = Left
    | Right
    | Up
    | Down
    | On


opposite : RelativePosition -> RelativePosition
opposite pos =
    case pos of
        Left ->
            Right

        Right ->
            Left

        Up ->
            Down

        Down ->
            Up

        On ->
            On


{-| The position of `a` relative to `b`
-}
relativePosition : TextRect a -> TextRect a -> Maybe RelativePosition
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


keepEdgeInPlace : RelativePosition -> TextRect a -> TextRect a -> TextRect a
keepEdgeInPlace edge old new =
    let
        ( oldSize, newSize ) =
            ( size old, size new )

        ( x0, y0 ) =
            old.position
    in
        case edge of
            On ->
                { new | position = old.position }

            Left ->
                { new | position = ( x0 - oldSize.width / 2 + newSize.width / 2, y0 ) }

            Right ->
                { new | position = ( x0 + oldSize.width / 2 - newSize.width / 2, y0 ) }

            Up ->
                { new | position = ( x0, y0 - oldSize.height / 2 + newSize.height / 2 ) }

            Down ->
                { new | position = ( x0, y0 + oldSize.height / 2 - newSize.height / 2 ) }
