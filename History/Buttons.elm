module History.Buttons
    exposing
        ( HistoryButtons
        , buttons
        , toList
        , fromList
        , updateEnabled
        , reposition
        , view
        )

import History exposing (History)
import Button exposing (Button)
import Util exposing (Size)
import TextRect
import Collage exposing (Collage, group)


type alias HistoryButtons msg =
    { undo : Button msg
    , redo : Button msg
    , clear : Button msg
    }


buttons : (History.Msg -> msg) -> History a -> HistoryButtons msg
buttons updateHistory history =
    { undo =
        Button.button "undo"
            (updateHistory History.Undo)
            (History.canUndo history)
    , redo =
        Button.button "redo"
            (updateHistory History.Redo)
            (History.canRedo history)
    , clear =
        Button.button "clear"
            (updateHistory History.Clear)
            (not <| History.isInitial history)
    }


toList : HistoryButtons msg -> List (Button msg)
toList { undo, redo, clear } =
    [ undo, redo, clear ]


fromList : List (Button msg) -> HistoryButtons msg
fromList buttons =
    case buttons of
        [ undo, redo, clear ] ->
            { undo = undo, redo = redo, clear = clear }

        _ ->
            Debug.crash "Oops"


updateEnabled : History a -> HistoryButtons msg -> HistoryButtons msg
updateEnabled history { undo, redo, clear } =
    { undo = { undo | enabled = History.canUndo history }
    , redo = { redo | enabled = History.canRedo history }
    , clear = { clear | enabled = not <| History.isInitial history }
    }


reposition : Size -> Size -> HistoryButtons msg -> HistoryButtons msg
reposition area padding buttons =
    let
        y0 =
            buttons
                |> toList
                |> List.map (TextRect.size >> .height)
                |> List.maximum
                |> Maybe.withDefault 0
                |> (+) (2 * padding.height)
    in
        TextRect.organizeInRows (area.height - y0)
            area
            padding
            (toList buttons)
            |> Tuple.first
            |> fromList


view : HistoryButtons msg -> Collage msg
view buttons =
    buttons
        |> toList
        |> List.map Button.view
        |> group
