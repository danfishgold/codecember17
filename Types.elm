module Types exposing (Size, Edges, Collage)


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
