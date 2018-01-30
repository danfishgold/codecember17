module Magnet.Category
    exposing
        ( Category
        , category
        , insert
        , merge
        , allSources
        , filterFirst
        , filterMapFirst
        )

import Magnet.Base as Base exposing (Magnet)
import Color
import Util


type alias Category data =
    { name : String
    , sources : List (Magnet data)
    }


category : String -> List ( String, Int ) -> Category {}
category name stringsAndSizes =
    stringsAndSizes
        |> List.map (\( str, sz ) -> Base.magnet str sz (Base.data Color.black Color.white))
        |> \sources -> { name = name, sources = sources }


insert : Category data -> List (Category data) -> List (Category data)
insert cat cats =
    case cats of
        [] ->
            [ cat ]

        first :: rest ->
            if first.name == cat.name then
                let
                    datas =
                        List.map .data first.sources

                    newSources =
                        cat.sources
                            |> List.filter (not << flip List.member datas << .data)
                            |> \new -> first.sources ++ new
                in
                    { first | sources = newSources } :: rest
            else
                first :: insert cat rest


merge : List (Category data) -> List (Category data) -> List (Category data)
merge cats1 cats2 =
    List.foldl insert cats2 cats1


allSources : List (Category data) -> List (Magnet data)
allSources categories =
    List.concatMap .sources categories


filterFirst : (Magnet data -> Bool) -> List (Category data) -> ( List (Category data), Maybe (Magnet data) )
filterFirst fn categories =
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
        recurse categories []


filterMapFirst : (Magnet data -> Maybe b) -> List (Category data) -> ( List (Category data), Maybe ( Magnet data, b ) )
filterMapFirst fn categories =
    let
        recurse cats falses =
            case cats of
                [] ->
                    ( List.reverse falses, Nothing )

                head :: rest ->
                    case Util.filterMapFirst fn head.sources of
                        ( _, Nothing ) ->
                            recurse rest (head :: falses)

                        ( otherSources, Just success ) ->
                            ( List.concat
                                [ List.reverse falses
                                , [ { head | sources = otherSources } ]
                                , rest
                                ]
                            , Just success
                            )
    in
        recurse categories []
