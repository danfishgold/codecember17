module Util exposing (Size, Edges, Collage, filterFirst, maybeOr)


type alias Size =
    { width : Float
    , height : Float
    }


type alias Edges =
    { minX : Float
    , maxX : Float
    , minY : Float
    , maxY : Float
    }


type alias Collage a =
    { a
        | width : Float
        , height : Float
    }


filterFirst : (a -> Bool) -> List a -> ( List a, Maybe a )
filterFirst fn xs =
    let
        recurse lst falses =
            case lst of
                [] ->
                    ( List.reverse falses, Nothing )

                head :: rest ->
                    if fn head then
                        ( List.reverse falses ++ rest, Just head )
                    else
                        recurse rest (head :: falses)
    in
        recurse xs []


maybeOr : (() -> Maybe a) -> Maybe a -> Maybe a
maybeOr lazyOther current =
    if current == Nothing then
        lazyOther ()
    else
        current
