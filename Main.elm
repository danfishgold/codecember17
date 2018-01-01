module Main exposing (..)

import Magnet exposing (Magnet, magnet)
import Html exposing (Html, program)
import Collage exposing (group, rectangle, circle, shift, filled, uniform)
import Collage.Render exposing (svgBox)
import Window
import Color
import Task
import Pointer
import Array exposing (Array)
import Point exposing (Point)


type alias Model =
    { magnets : Array (Magnet ())
    , draggingMagnet : Maybe Int
    , size : { width : Float, height : Float }
    , pointer : Maybe Point
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
            Array.fromList
                [ magnet "Hey" ( 0, 0 )
                , magnet "Hi" ( 100, 100 )
                ]
      , draggingMagnet = Nothing
      , size = { width = 0, height = 0 }
      , pointer = Nothing
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
                , pointer =
                    Just pointer
                , draggingMagnet = indexOfFirst (Magnet.contains pointer) model.magnets
              }
            , Cmd.none
            )

        PointerMove pointer ->
            ( { model
                | pointer =
                    Just pointer
                , magnets =
                    case model.pointer of
                        Nothing ->
                            model.magnets

                        Just oldPointer ->
                            dragMagnet model.magnets model.draggingMagnet (Point.sub pointer oldPointer)
              }
            , Cmd.none
            )

        PointerEnd pointer ->
            ( { model
                | mouseDown = False
                , pointer =
                    Just pointer
                , draggingMagnet = Nothing
              }
            , Cmd.none
            )


dragMagnet : Array (Magnet ()) -> Maybe Int -> Point -> Array (Magnet ())
dragMagnet magnets draggingIndex delta =
    case draggingIndex of
        Nothing ->
            magnets

        Just idx ->
            updateArray (Magnet.moveBy delta)
                idx
                magnets


view : Model -> Html Msg
view model =
    let
        magnets =
            model.magnets
                |> Array.map Magnet.view
                |> Array.toList
                |> group

        bg =
            rectangle model.size.width model.size.height |> filled (uniform Color.lightGray)

        pointer =
            case model.pointer of
                Just p ->
                    circle 5
                        |> filled (uniform Color.red)
                        |> shift p

                Nothing ->
                    group []
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


indexOfFirst : (a -> Bool) -> Array a -> Maybe Int
indexOfFirst fn xs =
    let
        recurse fn xs len idx =
            if idx < len then
                case Array.get idx xs of
                    Nothing ->
                        Nothing

                    Just x ->
                        if fn x then
                            Just idx
                        else
                            recurse fn xs len (idx + 1)
            else
                Nothing
    in
        recurse fn xs (Array.length xs) 0


updateArray : (a -> a) -> Int -> Array a -> Array a
updateArray fn idx xs =
    case Array.get idx xs of
        Nothing ->
            xs

        Just x ->
            Array.set idx (fn x) xs
