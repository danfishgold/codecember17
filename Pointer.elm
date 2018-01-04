port module Pointer
    exposing
        ( Event
        , DragState(..)
        , Identifier
        , Pointer
        , idToString
        , identifier
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


type alias Collage a =
    { a | width : Float, height : Float }


type alias PortEvent =
    { pointers : List PortPointer
    , ctrlDown : Bool
    }


type Identifier
    = Identifier String


identifier : String -> Identifier
identifier =
    Identifier


idToString : Identifier -> String
idToString (Identifier string) =
    string


type alias PortPointer =
    { identifier : String
    , position : Point
    }


type alias Pointer =
    { identifier : Identifier
    , position : Point
    }


type alias Event =
    { pointers : List Pointer
    , ctrlDown : Bool
    , state : DragState
    }


type DragState
    = Start
    | Move
    | End
    | Cancel


importPointer : PortPointer -> Pointer
importPointer p =
    { p | identifier = Identifier p.identifier }


event : DragState -> PortEvent -> Event
event state { pointers, ctrlDown } =
    { pointers = List.map importPointer pointers
    , ctrlDown = ctrlDown
    , state = state
    }


port mouseDown : (PortEvent -> msg) -> Sub msg


port mouseMove : (PortEvent -> msg) -> Sub msg


port mouseUp : (PortEvent -> msg) -> Sub msg


port touchStart : (PortEvent -> msg) -> Sub msg


port touchMove : (PortEvent -> msg) -> Sub msg


port touchEnd : (PortEvent -> msg) -> Sub msg


port touchCancel : (PortEvent -> msg) -> Sub msg


cancel : (PortEvent -> msg) -> Sub msg
cancel toMsg =
    touchCancel toMsg


start : (PortEvent -> msg) -> Sub msg
start toMsg =
    Sub.batch
        [ touchStart toMsg
        , mouseDown toMsg
        ]


move : (PortEvent -> msg) -> Sub msg
move toMsg =
    Sub.batch
        [ touchMove toMsg
        , mouseMove toMsg
        ]


end : (PortEvent -> msg) -> Sub msg
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


inCollage : Collage a -> Point -> Point
inCollage { width, height } ( x, y ) =
    ( x - width / 2, height / 2 - y )


pointerInCollage : Collage a -> Pointer -> Pointer
pointerInCollage collage pointer =
    { pointer | position = inCollage collage pointer.position }


eventInCollage : Collage a -> Event -> Event
eventInCollage collage event =
    { event | pointers = List.map (pointerInCollage collage) event.pointers }


eventsInCollage : Collage a -> (Event -> msg) -> Sub msg
eventsInCollage collage toMsg =
    events (eventInCollage collage >> toMsg)
