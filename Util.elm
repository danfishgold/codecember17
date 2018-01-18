module Util
    exposing
        ( Size
        , Edges
        , Collage
        , Direction(..)
        , filterFirst
        , filterMapFirst
        , maybeOr
        , between
        )


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


type Direction
    = Rtl
    | Ltr


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


filterMapFirst : (a -> Maybe b) -> List a -> ( List a, Maybe ( a, b ) )
filterMapFirst fn xs =
    let
        recurse lst falses =
            case lst of
                [] ->
                    ( List.reverse falses, Nothing )

                head :: rest ->
                    case fn head of
                        Just res ->
                            ( List.reverse falses ++ rest, Just ( head, res ) )

                        Nothing ->
                            recurse rest (head :: falses)
    in
        recurse xs []


maybeOr : (() -> Maybe a) -> Maybe a -> Maybe a
maybeOr lazyOther current =
    if current == Nothing then
        lazyOther ()
    else
        current


between : Float -> Float -> Float -> Bool
between a b x =
    a <= x && x <= b
