module Main exposing (main)

import Magnet exposing (Magnets)
import Magnet.Category exposing (category)
import Html exposing (Html, program)
import Collage exposing (group, rectangle, circle, shift, filled, uniform)
import Collage.Render exposing (svgBox)
import Color exposing (Color)
import Pointer exposing (Pointer)
import Pointer.Mapping exposing (Mapping)
import Util exposing (Size)
import History exposing (History)
import History.Buttons exposing (HistoryButtons)
import TextRect
import Button
import ElementSize


type alias Model =
    { magnets : History (Magnets {})
    , buttons : HistoryButtons Msg
    , size : Size
    , pointers : Mapping Pointer
    , ctrlDown : Bool
    , mouseDown : Bool
    }


type Msg
    = SetSize Size
    | PointerEvent Pointer.Event
    | UpdateHistory History.Msg


letters : List String
letters =
    "a b c d e f g h i j k l m n o p q r s t u v w x y z [space]" |> String.split " "


initialMagnets : History (Magnets {})
initialMagnets =
    History.initial
        { stationary =
            []
        , dragging = Pointer.Mapping.empty
        , sources =
            [ category "Letters" letters
            ]
        }


init : ( Model, Cmd Msg )
init =
    ( { magnets = initialMagnets
      , buttons = History.Buttons.buttons UpdateHistory initialMagnets
      , size = { width = 0, height = 0 }
      , pointers = Pointer.Mapping.empty
      , ctrlDown = False
      , mouseDown = False
      }
    , ElementSize.get
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ ElementSize.changes SetSize
        , Pointer.eventsInCollage model.size PointerEvent
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSize size ->
            ( { model | size = size } |> refreshElements, Cmd.none )

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

                buttonList =
                    History.Buttons.toList model.buttons

                ( ( newButtonList, remainingPointers ), buttonCmd ) =
                    case event.state of
                        Pointer.Start ->
                            ( Button.startClick newPointers buttonList, Cmd.none )

                        Pointer.Move ->
                            ( ( buttonList, newPointers ), Cmd.none )

                        Pointer.End ->
                            Button.endClick event.pointers buttonList

                        Pointer.Cancel ->
                            Button.endClick event.pointers buttonList

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
                    , buttons =
                        History.Buttons.fromList newButtonList
                  }
                    |> refreshElements
                , buttonCmd
                )

        UpdateHistory historyMsg ->
            ( { model | magnets = History.update historyMsg model.magnets }
                |> refreshElements
            , Cmd.none
            )


refreshElements : Model -> Model
refreshElements model =
    let
        newMagnets =
            History.modifyInPlace
                (Magnet.repositionSources model.size TextRect.defaultPadding)
                model.magnets
    in
        { model
            | magnets = newMagnets
            , buttons =
                History.Buttons.reposition model.size TextRect.defaultPadding model.buttons
                    |> History.Buttons.updateEnabled newMagnets
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
            Magnet.magnetsView model.magnets.current

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
            History.Buttons.view model.buttons
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
