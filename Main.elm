module Main exposing (..)

import Magnet exposing (Magnet)
import Html exposing (Html, program, text)
import Collage exposing (group)
import Collage.Render exposing (svg)


type alias Model =
    { magnets : List (Magnet ()) }


type Msg
    = Msg


init : ( Model, Cmd Msg )
init =
    ( { magnets = [ Magnet () "Hey" ] }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    model.magnets |> List.map Magnet.view |> group |> svg


main : Program Never Model Msg
main =
    program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
