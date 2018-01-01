port module Pointer
    exposing
        ( Event
        , start
        , move
        , end
        , cancel
        , inCollage
        )

import Point exposing (Point)


--


type alias Event =
    { pointer : Point
    , ctrlDown : Bool
    }


port mouseDown : (Event -> msg) -> Sub msg


port mouseMove : (Event -> msg) -> Sub msg


port mouseUp : (Event -> msg) -> Sub msg


port touchStart : (Event -> msg) -> Sub msg


port touchMove : (Event -> msg) -> Sub msg


port touchEnd : (Event -> msg) -> Sub msg


inCollage : { a | width : Float, height : Float } -> Point -> Point
inCollage { width, height } ( x, y ) =
    ( x - width / 2, height / 2 - y )


cancel : a -> Sub msg
cancel toMsg =
    Sub.none


start : (Event -> msg) -> Sub msg
start toMsg =
    Sub.batch
        [ touchStart toMsg
        , mouseDown toMsg
        ]


move : (Event -> msg) -> Sub msg
move toMsg =
    Sub.batch
        [ touchMove toMsg
        , mouseMove toMsg
        ]


end : (Event -> msg) -> Sub msg
end toMsg =
    Sub.batch
        [ touchEnd toMsg
        , mouseUp toMsg
        ]
