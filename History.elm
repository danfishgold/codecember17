module History
    exposing
        ( History
        , empty
        , canUndo
        , canRedo
        , addLatest
        , undo
        , redo
        )


type alias History a =
    { previous : List a
    , next : List a
    }


empty : History a
empty =
    { previous = []
    , next = []
    }


canUndo : History a -> Bool
canUndo history =
    not <| List.isEmpty history.previous


canRedo : History a -> Bool
canRedo history =
    not <| List.isEmpty history.next


addLatest : a -> History a -> History a
addLatest latest history =
    { history
        | previous = latest :: history.previous
        , next = []
    }


undo : History a -> Maybe ( History a, a )
undo history =
    case history.previous of
        latest :: older ->
            Just
                ( { history
                    | previous = older
                    , next = latest :: history.next
                  }
                , latest
                )

        [] ->
            Nothing


redo : History a -> Maybe ( History a, a )
redo history =
    case history.next of
        upNext :: newer ->
            Just
                ( { history
                    | next = newer
                    , previous = upNext :: history.previous
                  }
                , upNext
                )

        [] ->
            Nothing
