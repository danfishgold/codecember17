module Main exposing (main)

import Magnet exposing (Magnets)
import Html exposing (Html, programWithFlags)
import Collage exposing (group, rectangle, filled, uniform)
import Collage.Render exposing (svgBox)
import Color
import Pointer exposing (Pointer)
import Pointer.Mapping exposing (Mapping)
import Util exposing (Size, Direction(..))
import History exposing (History)
import History.Buttons exposing (HistoryButtons)
import TextRect
import Button
import ElementSize
import English
import Hebrew
import Emoji


type alias Model data =
    { magnets : History (Magnets data)
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


init : Magnet.Environment data -> ( Model data, Cmd Msg )
init env =
    let
        magnets =
            History.initial (Magnet.initial env.sources)
    in
        ( { magnets = magnets
          , buttons = History.Buttons.buttons UpdateHistory magnets
          , size = { width = 0, height = 0 }
          , pointers = Pointer.Mapping.empty
          , ctrlDown = False
          , mouseDown = False
          }
        , ElementSize.get
        )


subscriptions : Model data -> Sub Msg
subscriptions model =
    Sub.batch
        [ ElementSize.changes SetSize
        , Pointer.eventsInCollage model.size PointerEvent
        ]


update : Magnet.Environment data -> Msg -> Model data -> ( Model data, Cmd Msg )
update { interaction, sourcesDirection } msg model =
    case msg of
        SetSize size ->
            ( { model | size = size } |> refreshElements sourcesDirection, Cmd.none )

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
                            Magnet.keepDragging interaction
                                model.pointers
                                remainingPointers

                        Pointer.End ->
                            Magnet.stopDragging interaction
                                event.pointers

                        Pointer.Cancel ->
                            Magnet.stopDragging interaction
                                event.pointers

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
                    |> refreshElements sourcesDirection
                , buttonCmd
                )

        UpdateHistory historyMsg ->
            ( { model | magnets = History.update historyMsg model.magnets }
                |> refreshElements sourcesDirection
            , Cmd.none
            )


refreshElements : Direction -> Model data -> Model data
refreshElements sourcesDirection model =
    let
        newMagnets =
            History.modifyInPlace
                (Magnet.repositionSources sourcesDirection model.size TextRect.defaultPadding)
                model.magnets
    in
        { model
            | magnets = newMagnets
            , buttons =
                History.Buttons.reposition Ltr model.size TextRect.defaultPadding model.buttons
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


view : Model data -> Html Msg
view model =
    let
        magnets =
            Magnet.magnetsView model.magnets.current

        bg =
            rectangle model.size.width model.size.height
                |> filled (uniform Color.lightGray)

        buttons =
            History.Buttons.view model.buttons
    in
        group [ magnets, buttons, bg ]
            |> svgBox ( model.size.width, model.size.height )


type ModelWrapper
    = English (Model English.Data)
    | Hebrew (Model Hebrew.Data)
    | Emoji (Model Emoji.Data)


initWrapper : String -> ( ModelWrapper, Cmd Msg )
initWrapper flag =
    case flag of
        "english" ->
            init English.environment |> Tuple.mapFirst English

        "hebrew" ->
            init Hebrew.environment |> Tuple.mapFirst Hebrew

        _ ->
            init Emoji.environment |> Tuple.mapFirst Emoji


subscriptionsWrapper : ModelWrapper -> Sub Msg
subscriptionsWrapper wrapper =
    case wrapper of
        English model ->
            subscriptions model

        Hebrew model ->
            subscriptions model

        Emoji model ->
            subscriptions model


updateWrapper : Msg -> ModelWrapper -> ( ModelWrapper, Cmd Msg )
updateWrapper msg wrapper =
    case wrapper of
        English model ->
            update English.environment msg model |> Tuple.mapFirst English

        Hebrew model ->
            update Hebrew.environment msg model |> Tuple.mapFirst Hebrew

        Emoji model ->
            update Emoji.environment msg model |> Tuple.mapFirst Emoji


viewWrapper : ModelWrapper -> Html Msg
viewWrapper wrapper =
    case wrapper of
        English model ->
            view model

        Hebrew model ->
            view model

        Emoji model ->
            view model


main : Program String ModelWrapper Msg
main =
    programWithFlags
        { init = initWrapper
        , subscriptions = subscriptionsWrapper
        , update = updateWrapper
        , view = viewWrapper
        }
