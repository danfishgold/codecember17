module Main exposing (..)

import Magnet exposing (Magnet, Magnets, magnet)
import Html exposing (Html, program)
import Collage exposing (group, rectangle, circle, shift, filled, uniform)
import Collage.Render exposing (svgBox)
import Window
import Color exposing (Color)
import Task
import Pointer
import Pointer.Mapping exposing (Mapping)
import Point exposing (Point)


type alias Model =
    { magnets : Magnets Color
    , size : { width : Float, height : Float }
    , pointers : Mapping Point
    , ctrlDown : Bool
    , mouseDown : Bool
    }


type Msg
    = SetSize Window.Size
    | PointerChange Pointer.Event


letters : List String
letters =
    [ "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "_" ]


magnetGroup : List String -> List (Magnet Color)
magnetGroup strings =
    strings
        |> List.indexedMap
            (\idx str ->
                magnet str
                    (magnetPosition (List.length strings) idx)
                    Color.black
            )


magnetPosition : Int -> Int -> ( Float, Float )
magnetPosition count idx =
    let
        sideLength =
            ceiling <| sqrt <| toFloat count

        row =
            -(idx // sideLength) + sideLength

        col =
            idx % sideLength
    in
        ( toFloat col * 30, toFloat row * 50 )


init : ( Model, Cmd Msg )
init =
    ( { magnets =
            { stationary =
                []
            , dragging = Pointer.Mapping.empty
            , sources =
                magnetGroup letters
            }
      , size = { width = 0, height = 0 }
      , pointers = Pointer.Mapping.empty
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

                newPointers =
                    updatePointers event model.pointers

                newMagnets =
                    case event.state of
                        Pointer.Start ->
                            Magnet.startDragging newPointers model.magnets

                        Pointer.Move ->
                            Magnet.keepDragging model.pointers newPointers model.magnets

                        Pointer.End ->
                            Magnet.stopDragging (List.map .id event.pointers) model.magnets

                        Pointer.Cancel ->
                            Magnet.stopDragging (List.map .id event.pointers) model.magnets
            in
                ( { model
                    | mouseDown = mouseDown
                    , pointers = newPointers
                    , magnets = newMagnets
                  }
                , Cmd.none
                )


updatePointers : Pointer.Event -> Mapping Point -> Mapping Point
updatePointers event pointers =
    let
        add { id, position } =
            Pointer.Mapping.add id position

        remove { id } =
            Pointer.Mapping.remove id
    in
        case event.state of
            Pointer.Start ->
                List.foldl add pointers event.pointers

            Pointer.Move ->
                List.foldl add pointers event.pointers

            Pointer.End ->
                List.foldl remove pointers event.pointers

            Pointer.Cancel ->
                List.foldl remove pointers event.pointers


view : Model -> Html Msg
view model =
    let
        magnets =
            Magnet.magnetsView .data model.magnets

        bg =
            rectangle model.size.width model.size.height |> filled (uniform Color.lightGray)

        pointers =
            model.pointers
                |> Pointer.Mapping.toList
                |> List.map
                    (\p ->
                        circle 5
                            |> filled (uniform Color.red)
                            |> shift p
                    )
                |> group
    in
        group [ pointers, magnets, bg ]
            |> svgBox ( model.size.width, model.size.height )


main : Program Never Model Msg
main =
    program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
