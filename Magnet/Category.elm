module Magnet.Category exposing (..)

import Magnet.Base exposing (Magnet)
import Util


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


filterFirst : (Magnet data -> Bool) -> List (Category data) -> ( List (Category data), Maybe (Magnet data) )
filterFirst fn cats =
    let
        recurse cats falses =
            case cats of
                [] ->
                    ( List.reverse falses, Nothing )

                head :: rest ->
                    case Util.filterFirst fn head.sources of
                        ( _, Nothing ) ->
                            recurse rest (head :: falses)

                        ( otherSources, Just successfulSource ) ->
                            ( List.concat
                                [ List.reverse falses
                                , [ { head | sources = otherSources } ]
                                , rest
                                ]
                            , Just successfulSource
                            )
    in
        recurse cats []
