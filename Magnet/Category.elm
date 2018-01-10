module Magnet.Category exposing (..)

import Magnet.Base exposing (Magnet)


type alias Category data =
    { name : String
    , sources : List (Magnet data)
    }


insert : Category data -> List (Category data) -> List (Category data)
insert cat cats =
    case cats of
        [] ->
            [ cat ]

        first :: rest ->
            if first.name == cat.name then
                { first | sources = cat.sources ++ first.sources } :: rest
            else
                first :: insert cat rest


merge : List (Category data) -> List (Category data) -> List (Category data)
merge cats1 cats2 =
    List.foldl insert cats2 cats1


allSources : List (Category data) -> List (Magnet data)
allSources categories =
    List.concatMap .sources categories
