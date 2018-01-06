module Main exposing (main)

import Magnet exposing (Magnets)
import Html exposing (Html, program)
import Collage exposing (group, rectangle, circle, shift, filled, uniform)
import Collage.Render exposing (svgBox)
import Window
import Color exposing (Color)
import Task
import Pointer exposing (Pointer)
import Pointer.Mapping exposing (Mapping)
import Types exposing (Size)
import History exposing (History)
import TextRect
import Button exposing (Button)


type alias Model =
    { magnets : History (Magnets Color)
    , buttons : List (Button Msg)
    , size : Size
    , pointers : Mapping Pointer
    , ctrlDown : Bool
    , mouseDown : Bool
    }


type Msg
    = SetSize Window.Size
    | PointerEvent Pointer.Event
    | UpdateHistory History.Msg


letters : List String
letters =
    "a b c d e f g h i j k l m n o p q r s t u v w x y z [space]" |> String.split " "


initialMagnets : History (Magnets Color)
initialMagnets =
    History.initial
        { stationary =
            []
        , dragging = Pointer.Mapping.empty
        , sources =
            [ Magnet.category "Letters" letters
            , Magnet.category "Nouns" [ "burrito", "pizza" ]
            , Magnet.category "Verbs" [ "eat", "drink", "go" ]
            ]
        }


init : ( Model, Cmd Msg )
init =
    ( { magnets = initialMagnets
      , buttons = History.buttons UpdateHistory initialMagnets
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
        , Pointer.eventsInCollage model.size PointerEvent
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSize { width, height } ->
            let
                newSize =
                    { width = toFloat width
                    , height = toFloat height
                    }
            in
                ( { model | size = newSize } |> repositionElements, Cmd.none )

        PointerEvent event ->
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

                ( ( newButtons, remainingPointers ), buttonCmd ) =
                    case event.state of
                        Pointer.Start ->
                            ( Button.startClick newPointers model.buttons, Cmd.none )

                        Pointer.Move ->
                            ( ( model.buttons, newPointers ), Cmd.none )

                        Pointer.End ->
                            Button.endClick event.pointers model.buttons

                        Pointer.Cancel ->
                            Button.endClick event.pointers model.buttons

                magnetModifier =
                    case event.state of
                        Pointer.Start ->
                            Magnet.startDragging remainingPointers

                        Pointer.Move ->
                            Magnet.keepDragging model.pointers remainingPointers

                        Pointer.End ->
                            Magnet.stopDragging event.pointers

                        Pointer.Cancel ->
                            Magnet.stopDragging event.pointers

                newMagnets =
                    if event.state == Pointer.Start then
                        History.modify magnetModifier model.magnets
                    else
                        History.modifyInPlace magnetModifier model.magnets
            in
                ( { model
                    | mouseDown = mouseDown
                    , pointers = newPointers
                    , magnets = newMagnets
                    , buttons = newButtons
                  }
                , buttonCmd
                )

        UpdateHistory historyMsg ->
            ( { model | magnets = History.update historyMsg model.magnets }
                |> repositionElements
            , Cmd.none
            )


repositionElements : Model -> Model
repositionElements model =
    { model
        | magnets =
            History.modifyInPlace
                (Magnet.repositionSources model.size TextRect.defaultPadding)
                model.magnets
        , buttons =
            Button.repositionButtons model.size TextRect.defaultPadding model.buttons
    }


updatePointers : Pointer.Event -> Mapping Pointer -> Mapping Pointer
updatePointers event pointers =
    case event.state of
        Pointer.Start ->
            Pointer.Mapping.union event.pointers pointers

        Pointer.Move ->
            Pointer.Mapping.union event.pointers pointers

        Pointer.End ->
            Pointer.Mapping.subtract pointers event.pointers

        Pointer.Cancel ->
            Pointer.Mapping.subtract pointers event.pointers


view : Model -> Html Msg
view model =
    let
        magnets =
            Magnet.magnetsView .data model.magnets.current

        bg =
            rectangle model.size.width model.size.height
                |> filled (uniform Color.lightGray)

        pointers =
            model.pointers
                |> Pointer.Mapping.toList
                |> List.map
                    (\{ position } ->
                        circle 5
                            |> filled (uniform Color.red)
                            |> shift position
                    )
                |> group

        buttons =
            Button.buttonsView model.size model.buttons
    in
        group [ pointers, magnets, buttons, bg ]
            |> svgBox ( model.size.width, model.size.height )


main : Program Never Model Msg
main =
    program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
