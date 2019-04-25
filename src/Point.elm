module Point exposing (Point, add, div, mul, sub)


type alias Point =
    ( Float, Float )


sub : Point -> Point -> Point
sub ( x1, y1 ) ( x2, y2 ) =
    ( x1 - x2, y1 - y2 )


add : Point -> Point -> Point
add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


div : Point -> Float -> Point
div ( x, y ) f =
    ( x / f, y / f )


mul : Point -> Float -> Point
mul ( x, y ) f =
    ( x * f, y * f )
