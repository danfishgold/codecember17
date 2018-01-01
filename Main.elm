module Main exposing (..)

import Magnet exposing (Magnet)
import Html exposing (Html, program, text)
import Collage exposing (group, rectangle, filled, uniform)
import Collage.Render exposing (svgBox)
import Window
import Color
import Task


type alias Model =
    { magnets : List (Magnet ())
    , width : Float
    , height : Float
    }


type Msg
    = SetSize Window.Size


init : ( Model, Cmd Msg )
init =
    ( { magnets =
            [ Magnet () "Hey" ( 0, 0 )
            , Magnet () "Hi" ( 100, 100 )
            ]
      , width = 0
      , height = 0
      }
    , Task.perform SetSize Window.size
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Window.resizes SetSize ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSize { width, height } ->
            ( { model
                | width = toFloat width
                , height = toFloat height
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        magnets =
            model.magnets
                |> List.map Magnet.view
                |> group

        bg =
            rectangle model.width model.height |> filled (uniform Color.lightGray)
    in
        group [ magnets, bg ]
            |> svgBox ( model.width, model.height )


main : Program Never Model Msg
main =
    program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
