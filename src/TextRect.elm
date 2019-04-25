module TextRect exposing
    ( TextRect
    , centerPositionsForRows
    , contains
    , defaultPadding
    , edges
    , listCenter
    , moveBy
    , organizeInRowAround
    , organizeInRows
    , rect
    , size
    , view
    )

import Collage exposing (Collage, filled, group, rectangle, rendered, shift, uniform)
import Collage.Layout as Layout
import Collage.Text as Text
import Color exposing (Color)
import Point exposing (Point)
import Util exposing (Direction(..), Edges, Size)


type alias TextRect a =
    { a
        | position : Point
        , padding : Size
        , text : String
        , textSize : Int
    }


rect : String -> Int -> TextRect {}
rect text textSize =
    { position = ( 0, 0 )
    , padding = defaultPadding
    , text = text
    , textSize = textSize
    }


defaultPadding : Size
defaultPadding =
    { width = 15, height = 15 }


textText : String -> Int -> Text.Text
textText text size =
    text
        |> Text.fromString
        |> Text.size size


view : Color -> Color -> TextRect a -> Collage msg
view background textColor { padding, text, position, textSize } =
    let
        textNode =
            textText text textSize
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
        |> shift position


size : TextRect a -> Size
size { padding, text, textSize } =
    let
        textNode =
            textText text textSize |> rendered
    in
    { width = Layout.width textNode + padding.width
    , height = Layout.height textNode + padding.height
    }


edges : TextRect a -> Edges
edges rect =
    let
        { width, height } =
            size rect

        ( x0, y0 ) =
            rect.position
    in
    { minX = x0 - width / 2
    , maxX = x0 + width / 2
    , minY = y0 - height / 2
    , maxY = y0 + height / 2
    }


contains : Point -> TextRect a -> Bool
contains ( x, y ) rect =
    let
        r =
            edges rect
    in
    r.minX <= x && x <= r.maxX && r.minY <= y && y <= r.maxY


moveBy : Point -> TextRect a -> TextRect a
moveBy delta rect =
    { rect | position = Point.add rect.position delta }


centerPositionsForRows : Direction -> Float -> Size -> Size -> List (TextRect a) -> ( List Point, Float )
centerPositionsForRows dir offsetY area padding rects =
    let
        sizes =
            List.map size rects

        rowHeight =
            sizes
                |> List.map .height
                |> List.maximum
                |> Maybe.withDefault 0
                |> (+) padding.height

        insideCollage ( x0, y0 ) size =
            case dir of
                Ltr ->
                    ( x0 + size.width / 2 - area.width / 2
                    , area.height / 2 - (offsetY + y0 + size.height / 2)
                    )

                Rtl ->
                    ( area.width / 2 - (x0 + size.width / 2)
                    , area.height / 2 - (offsetY + y0 + size.height / 2)
                    )

        shouldPlaceHere currentX size =
            -- can fit in current row
            (currentX + 2 * padding.width + size.width <= area.width)
                -- or (can or can't fit && at beginning of row)
                || (currentX == 0)

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


organizeInRows : Direction -> Float -> Size -> Size -> List (TextRect a) -> ( List (TextRect a), Float )
organizeInRows dir offsetY area padding rects =
    if List.isEmpty rects then
        ( [], offsetY )

    else
        let
            ( positions, nextY ) =
                centerPositionsForRows dir
                    offsetY
                    area
                    padding
                    rects
        in
        ( List.map2 (\rect position -> { rect | position = position })
            rects
            positions
        , nextY
        )


listCenter : List (TextRect a) -> Point
listCenter rects =
    let
        edges_ =
            List.map edges rects

        minX =
            edges_ |> List.map .minX |> List.minimum

        maxX =
            edges_ |> List.map .maxX |> List.maximum

        minY =
            edges_ |> List.map .minY |> List.minimum

        maxY =
            edges_ |> List.map .maxY |> List.maximum

        center x0 x1 y0 y1 =
            ( (x0 + x1) / 2, (y0 + y1) / 2 )
    in
    Maybe.map4 center minX maxX minY maxY
        |> Maybe.withDefault ( 0, 0 )


organizeInRowAround : Direction -> Point -> Float -> List (TextRect a) -> List (TextRect a)
organizeInRowAround dir ( x0, y0 ) padding rects =
    case dir of
        Rtl ->
            organizeInRowAround Ltr ( x0, y0 ) padding (List.reverse rects)

        Ltr ->
            let
                widths =
                    List.map (size >> .width) rects

                totalWidth =
                    List.sum widths + padding * toFloat (List.length rects - 1)

                offset =
                    x0 - totalWidth / 2

                rights =
                    List.scanl (\width prevX -> prevX + padding + width) offset widths

                centerXs =
                    List.map2 (\right width -> right + width / 2) rights widths
            in
            List.map2 (\x rect -> { rect | position = ( x, y0 ) }) centerXs rects
