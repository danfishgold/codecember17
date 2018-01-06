module History
    exposing
        ( History
        , initial
        , buttons
        , modify
        , modifyInPlace
        )

import TextRect
import Collage exposing (Collage, shift, group)
import Collage.Events
import Types exposing (Size)
import Color


type alias History a =
    { previous : List a
    , current : a
    , next : List a
    }


initial : a -> History a
initial base =
    { previous = []
    , current = base
    , next = []
    }


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


reset : History a -> History a
reset history =
    case List.head <| List.drop (List.length history.previous - 1) history.previous of
        Just base ->
            initial base

        Nothing ->
            initial history.current


buttons : (History a -> msg) -> Size -> History a -> Collage msg
buttons setHistory area history =
    let
        rects =
            [ "undo", "redo", "reset" ]
                |> List.map TextRect.rect
                |> TextRect.organizeInRows (area.height - 100)
                    area
                    TextRect.defaultPadding
                |> Tuple.first

        enabled =
            [ canUndo history
            , canRedo history
            , not (isInitial history)
            ]

        msgs =
            [ setHistory (undo history)
            , setHistory (redo history)
            , setHistory (reset history)
            ]

        elements =
            rects
                |> List.map (TextRect.view Color.white Color.black)
                |> List.map2 Collage.Events.onClick msgs
                |> filter enabled
    in
        group elements


filter : List Bool -> List a -> List a
filter mask xs =
    List.map2
        (\ok x ->
            if ok then
                Just x
            else
                Nothing
        )
        mask
        xs
        |> List.filterMap identity
