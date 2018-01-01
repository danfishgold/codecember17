module Main exposing (..)

import Magnet exposing (Magnet, magnet)
import Html exposing (Html, program)
import Collage exposing (group, rectangle, circle, shift, filled, uniform)
import Collage.Render exposing (svgBox)
import Window
import Color
import Task
import Pointer
import Point exposing (Point)


type alias Magnets =
    { stationary : List (Magnet ())
    , dragging : Maybe (Magnet ())
    , sources : List (Magnet ())
    }


allMagnets : Magnets -> List (Magnet ())
allMagnets { stationary, dragging, sources } =
    stationary
        ++ sources
        ++ (Maybe.map List.singleton dragging |> Maybe.withDefault [])


stopDragging : Magnets -> Magnets
stopDragging magnets =
    case magnets.dragging of
        Nothing ->
            magnets

        Just m ->
            { magnets
                | stationary = m :: magnets.stationary
                , dragging = Nothing
            }


startDragging : Point -> Magnets -> Magnets
startDragging pointer magnets =
    let
        ( newStationary, draggingFromStationary ) =
            filterFirst (Magnet.contains pointer) magnets.stationary

        dragging =
            if draggingFromStationary == Nothing then
                filterFirst (Magnet.contains pointer) magnets.sources
                    |> Tuple.second
            else
                draggingFromStationary
    in
        { magnets | stationary = newStationary, dragging = dragging }


keepDragging : Point -> Point -> Magnets -> Magnets
keepDragging oldPointer newPointer magnets =
    case magnets.dragging of
        Nothing ->
            magnets

        Just m ->
            { magnets
                | dragging =
                    Magnet.moveBy (Point.sub newPointer oldPointer) m
                        |> Just
            }


type alias Model =
    { magnets : Magnets
    , size : { width : Float, height : Float }
    , pointer : Point
    , ctrlDown : Bool
    , mouseDown : Bool
    }


type Msg
    = SetSize Window.Size
    | PointerStart Point
    | PointerMove Point
    | PointerEnd Point


init : ( Model, Cmd Msg )
init =
    ( { magnets =
            { stationary =
                []
            , dragging = Nothing
            , sources =
                [ magnet "Hey" ( 0, 0 )
                , magnet "Hi" ( 100, 100 )
                ]
            }
      , size = { width = 0, height = 0 }
      , pointer = ( 0, 0 )
      , ctrlDown = False
      , mouseDown = False
      }
    , Task.perform SetSize Window.size
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes SetSize
        , Pointer.start (.pointer >> Pointer.inCollage model.size >> PointerStart)
        , Pointer.move (.pointer >> Pointer.inCollage model.size >> PointerMove)
        , Pointer.end (.pointer >> Pointer.inCollage model.size >> PointerEnd)
        , Pointer.cancel (.pointer >> Pointer.inCollage model.size >> PointerEnd)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSize { width, height } ->
            ( { model
                | size =
                    { width = toFloat width
                    , height = toFloat height
                    }
              }
            , Cmd.none
            )

        PointerStart pointer ->
            ( { model
                | mouseDown = True
                , pointer = pointer
                , magnets = startDragging pointer model.magnets
              }
            , Cmd.none
            )

        PointerMove newPointer ->
            ( { model
                | pointer = newPointer
                , magnets = keepDragging model.pointer newPointer model.magnets
              }
            , Cmd.none
            )

        PointerEnd pointer ->
            ( { model
                | mouseDown = False
                , pointer = pointer
                , magnets = stopDragging model.magnets
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        magnets =
            allMagnets model.magnets
                |> List.map Magnet.view
                |> group

        bg =
            rectangle model.size.width model.size.height |> filled (uniform Color.lightGray)

        pointer =
            circle 5
                |> filled (uniform Color.red)
                |> shift model.pointer
    in
        group [ pointer, magnets, bg ]
            |> svgBox ( model.size.width, model.size.height )


main : Program Never Model Msg
main =
    program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


filterFirst : (a -> Bool) -> List a -> ( List a, Maybe a )
filterFirst fn xs =
    let
        recurse fn xs falses =
            case xs of
                [] ->
                    ( falses, Nothing )

                head :: rest ->
                    if fn head then
                        ( rest ++ falses, Just head )
                    else
                        recurse fn rest (head :: falses)
    in
        recurse fn xs []
