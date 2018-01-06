module TextRect exposing (defaultPadding, view, size, centerPositionsForRows)

import Collage exposing (Collage, group, rendered, rectangle, filled, uniform)
import Collage.Text as Text
import Collage.Layout as Layout
import Types exposing (Size, Edges)
import Color exposing (Color)
import Point exposing (Point)


defaultPadding : Size
defaultPadding =
    { width = 15, height = 15 }


view : Color -> Color -> Size -> String -> Collage msg
view background textColor padding text =
    let
        textNode =
            text
                |> Text.fromString
                |> Text.color textColor
                |> rendered

        bgNode =
            rectangle
                (Layout.width textNode + padding.width)
                (Layout.height textNode + padding.height)
                |> filled (uniform background)
    in
        group
            [ textNode, bgNode ]


size : Size -> String -> Size
size padding text =
    let
        textNode =
            text
                |> Text.fromString
                |> Text.color Color.white
                |> rendered
    in
        { width = Layout.width textNode + padding.width
        , height = Layout.height textNode + padding.height
        }


centerPositionsForRows : Float -> Size -> Size -> Size -> List String -> ( List Point, Float )
centerPositionsForRows offsetY area padding rectPadding strings =
    let
        sizes =
            List.map (size rectPadding) strings

        rowHeight =
            sizes
                |> List.map .height
                |> List.maximum
                |> Maybe.withDefault 0
                |> (+) padding.height

        insideCollage ( x0, y0 ) size =
            ( x0 + size.width / 2 - area.width / 2
            , area.height / 2 - (offsetY + y0 + size.height / 2)
            )

        shouldPlaceHere currentX size =
            -- can fit in current row
            (currentX + 2 * padding.width + size.width <= area.width)
                -- or (can or can't fit && at beginning of row)
                ||
                    (currentX == 0)

        recurse : Point -> List Point -> List Size -> ( List Point, Float )
        recurse ( currentX, currentY ) finished remaining =
            case remaining of
                [] ->
                    ( finished, offsetY + currentY + rowHeight )

                size :: rest ->
                    if shouldPlaceHere currentX size then
                        recurse ( currentX + padding.width + size.width, currentY )
                            (insideCollage ( currentX + padding.width, currentY ) size
                                :: finished
                            )
                            rest
                    else
                        recurse
                            ( 0, currentY + rowHeight )
                            finished
                            remaining
    in
        recurse ( 0, padding.height ) [] sizes |> Tuple.mapFirst List.reverse
