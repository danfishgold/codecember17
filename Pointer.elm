port module Pointer
    exposing
        ( Event
        , DragState(..)
        , start
        , move
        , end
        , cancel
        , inCollage
        , events
        , eventsInCollage
        )

import Point exposing (Point)


--


type alias SpecificEvent =
    { pointer : Point
    , ctrlDown : Bool
    }


type alias Event =
    { pointer : Point
    , ctrlDown : Bool
    , state : DragState
    }


type DragState
    = Start
    | Move
    | End
    | Cancel


event : DragState -> SpecificEvent -> Event
event state { pointer, ctrlDown } =
    { pointer = pointer
    , ctrlDown = ctrlDown
    , state = state
    }


port mouseDown : (SpecificEvent -> msg) -> Sub msg


port mouseMove : (SpecificEvent -> msg) -> Sub msg


port mouseUp : (SpecificEvent -> msg) -> Sub msg


port touchStart : (SpecificEvent -> msg) -> Sub msg


port touchMove : (SpecificEvent -> msg) -> Sub msg


port touchEnd : (SpecificEvent -> msg) -> Sub msg


port touchCancel : (SpecificEvent -> msg) -> Sub msg


inCollage : { a | width : Float, height : Float } -> Point -> Point
inCollage { width, height } ( x, y ) =
    ( x - width / 2, height / 2 - y )


cancel : (SpecificEvent -> msg) -> Sub msg
cancel toMsg =
    touchCancel toMsg


start : (SpecificEvent -> msg) -> Sub msg
start toMsg =
    Sub.batch
        [ touchStart toMsg
        , mouseDown toMsg
        ]


move : (SpecificEvent -> msg) -> Sub msg
move toMsg =
    Sub.batch
        [ touchMove toMsg
        , mouseMove toMsg
        ]


end : (SpecificEvent -> msg) -> Sub msg
end toMsg =
    Sub.batch
        [ touchEnd toMsg
        , mouseUp toMsg
        ]


events : (Event -> msg) -> Sub msg
events toMsg =
    Sub.batch
        [ start (event Start >> toMsg)
        , move (event Move >> toMsg)
        , end (event End >> toMsg)
        , cancel (event Cancel >> toMsg)
        ]


eventInCollage : { a | width : Float, height : Float } -> Event -> Event
eventInCollage collage event =
    { event | pointer = inCollage collage event.pointer }


eventsInCollage : { a | width : Float, height : Float } -> (Event -> msg) -> Sub msg
eventsInCollage collage toMsg =
    events (eventInCollage collage >> toMsg)
