module Main exposing (..)

import Magnet exposing (Magnet, Magnets, magnet)
import Html exposing (Html, program)
import Collage exposing (group, rectangle, circle, shift, filled, uniform)
import Collage.Render exposing (svgBox)
import Window
import Color exposing (Color)
import Task
import Pointer
import Point exposing (Point)


type alias Model =
    { magnets : Magnets Color
    , size : { width : Float, height : Float }
    , pointer : Point
    , ctrlDown : Bool
    , mouseDown : Bool
    }


type Msg
    = SetSize Window.Size
    | PointerChange Pointer.Event


letters : List (Magnet Color)
letters =
    [ "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "_" ]
        |> List.indexedMap (\idx letter -> magnet letter (letterPosition idx) Color.black)


letterPosition : Int -> ( Float, Float )
letterPosition idx =
    let
        row =
            -(idx // 5) + 5

        col =
            idx % 5
    in
        ( toFloat col * 30, toFloat row * 50 )


init : ( Model, Cmd Msg )
init =
    ( { magnets =
            { stationary =
                []
            , dragging = Nothing
            , sources =
                letters
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
        , Pointer.eventsInCollage model.size PointerChange
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

        PointerChange event ->
            let
                mouseDown =
                    case event.state of
                        Pointer.Start ->
                            True

                        Pointer.End ->
                            False

                        _ ->
                            model.mouseDown
            in
                ( { model
                    | mouseDown = mouseDown
                    , pointer = event.pointer
                    , magnets = Magnet.drag model.pointer event model.magnets
                  }
                , Cmd.none
                )


view : Model -> Html Msg
view model =
    let
        magnets =
            Magnet.magnetsView .data model.magnets

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
