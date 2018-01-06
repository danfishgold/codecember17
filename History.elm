module History
    exposing
        ( History
        , Msg
        , initial
        , update
        , buttons
        , modify
        , modifyInPlace
        )

import Button exposing (Button)


type alias History a =
    { previous : List a
    , current : a
    , next : List a
    }


type Msg
    = Undo
    | Redo
    | Clear


initial : a -> History a
initial base =
    { previous = []
    , current = base
    , next = []
    }


update : Msg -> History a -> History a
update msg history =
    case msg of
        Undo ->
            undo history

        Redo ->
            redo history

        Clear ->
            clear history


canUndo : History a -> Bool
canUndo history =
    not <| List.isEmpty history.previous


canRedo : History a -> Bool
canRedo history =
    not <| List.isEmpty history.next


isInitial : History a -> Bool
isInitial history =
    List.isEmpty history.previous && List.isEmpty history.next


add : a -> History a -> History a
add latest history =
    if latest /= history.current then
        { history
            | previous = history.current :: history.previous
            , current = latest
            , next = []
        }
    else
        history


modify : (a -> a) -> History a -> History a
modify fn history =
    add (fn history.current) history


addInPlace : a -> History a -> History a
addInPlace latest history =
    { history | current = latest }


modifyInPlace : (a -> a) -> History a -> History a
modifyInPlace fn history =
    addInPlace (fn history.current) history


undo : History a -> History a
undo history =
    case history.previous of
        latest :: older ->
            { history
                | previous = older
                , current = latest
                , next = history.current :: history.next
            }

        [] ->
            history


redo : History a -> History a
redo history =
    case history.next of
        upNext :: newer ->
            { history
                | previous = history.current :: history.previous
                , current = upNext
                , next = newer
            }

        [] ->
            history


clear : History a -> History a
clear history =
    case List.head <| List.drop (List.length history.previous - 1) history.previous of
        Just base ->
            initial base

        Nothing ->
            initial history.current


buttons : (Msg -> msg) -> History a -> List (Button msg)
buttons updateHistory history =
    [ Button.button "undo"
        (updateHistory Undo)
        (canUndo history)
    , Button.button "redo"
        (updateHistory Redo)
        (canRedo history)
    , Button.button "clear"
        (updateHistory Clear)
        (not <| isInitial history)
    ]
