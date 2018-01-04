port module Pointer
    exposing
        ( Event
        , DragState(..)
        , Source(..)
        , Pointer
        , Id
        , inCollage
        , events
        , eventsInCollage
        )

import Point exposing (Point)
import Pointer.Id as Id
import Pointer.Mapping as Mapping exposing (Mapping)


--


type alias Collage a =
    { a | width : Float, height : Float }


type alias PortEvent =
    { pointers : List PortPointer
    , ctrlDown : Bool
    }


type alias PortPointer =
    { id : String
    , position : Point
    }


type alias Pointer =
    { id : Id
    , position : Point
    , source : Source
    }


type alias Event =
    { pointers : Mapping Pointer
    , ctrlDown : Bool
    , state : DragState
    }


type DragState
    = Start
    | Move
    | End
    | Cancel


type Source
    = Mouse
    | Touch


type alias Id =
    Id.Id


importPointer : Source -> PortPointer -> Pointer
importPointer source { position, id } =
    { position = position
    , id = Id.fromString id
    , source = source
    }


event : Source -> DragState -> PortEvent -> Event
event source state { pointers, ctrlDown } =
    { pointers =
        pointers
            |> List.map (importPointer source)
            |> List.foldl (\p -> Mapping.add p.id p) Mapping.empty
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


events : (Event -> msg) -> Sub msg
events toMsg =
    Sub.batch
        [ touchStart (event Touch Start >> toMsg)
        , mouseDown (event Mouse Start >> toMsg)
        , touchMove (event Touch Move >> toMsg)
        , mouseMove (event Mouse Move >> toMsg)
        , touchEnd (event Touch End >> toMsg)
        , mouseUp (event Mouse End >> toMsg)
        , touchCancel (event Touch Cancel >> toMsg)
        ]


inCollage : Collage a -> Point -> Point
inCollage { width, height } ( x, y ) =
    ( x - width / 2, height / 2 - y )


pointerInCollage : Collage a -> Pointer -> Pointer
pointerInCollage collage pointer =
    { pointer | position = inCollage collage pointer.position }


eventInCollage : Collage a -> Event -> Event
eventInCollage collage event =
    { event | pointers = Mapping.map (always (pointerInCollage collage)) event.pointers }


eventsInCollage : Collage a -> (Event -> msg) -> Sub msg
eventsInCollage collage toMsg =
    events (eventInCollage collage >> toMsg)
