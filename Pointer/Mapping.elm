module Pointer.Mapping
    exposing
        ( Mapping
        , fromDict
        , fromList
        , add
        , remove
        , extract
        , toList
        , map
        , mutualMap3
        , empty
        , foldl
        , ids
        , union
        , subtract
        )

import Pointer.Id exposing (Id)
import Dict exposing (Dict)


type Mapping a
    = Inner (Dict.Dict String a)


fromDict : Dict String a -> Mapping a
fromDict dict =
    Inner dict


add : Id -> a -> Mapping a -> Mapping a
add id a (Inner dict) =
    Inner (Dict.insert (Pointer.Id.toString id) a dict)


remove : Id -> Mapping a -> Mapping a
remove id (Inner dict) =
    Inner (Dict.remove (Pointer.Id.toString id) dict)


extract : List Id -> Mapping a -> ( Mapping a, List a )
extract ids (Inner dict) =
    let
        stringIds =
            List.map Pointer.Id.toString ids

        ( extracted, remaining ) =
            Dict.partition (\stringId _ -> List.member stringId stringIds) dict
    in
        ( Inner remaining, Dict.values extracted )


toList : Mapping a -> List a
toList (Inner dict) =
    Dict.values dict


fromList : (a -> Id) -> List a -> Mapping a
fromList keyFn xs =
    List.map (\x -> ( Pointer.Id.toString <| keyFn x, x )) xs
        |> Dict.fromList
        |> fromDict


map : (Id -> a -> b) -> Mapping a -> Mapping b
map fn (Inner dict) =
    dict
        |> Dict.map (\stringId value -> fn (Pointer.Id.fromString stringId) value)
        |> Inner


mutualMap3 : (a -> b -> c -> d) -> Mapping a -> Mapping b -> Mapping c -> Mapping d
mutualMap3 fn pa pb pc =
    mutualMap2
        (\( a, b ) c -> fn a b c)
        (mutualMap2 (,) pa pb)
        pc


mutualMap2 : (a -> b -> c) -> Mapping a -> Mapping b -> Mapping c
mutualMap2 fn (Inner pa) (Inner pb) =
    Dict.merge
        (always <| always identity)
        (\k a b dict -> Dict.insert k (fn a b) dict)
        (always <| always identity)
        pa
        pb
        Dict.empty
        |> fromDict


empty : Mapping a
empty =
    Inner Dict.empty


foldl : (Id -> a -> b -> b) -> b -> Mapping a -> b
foldl folder initial (Inner dict) =
    let
        newFolder stringId a b =
            folder (Pointer.Id.fromString stringId) a b
    in
        Dict.foldl newFolder initial dict


ids : Mapping a -> List Id
ids (Inner dict) =
    Dict.keys dict
        |> List.map Pointer.Id.fromString


union : Mapping a -> Mapping a -> Mapping a
union (Inner da) (Inner db) =
    Inner <| Dict.union da db


subtract : Mapping a -> Mapping a -> Mapping a
subtract (Inner da) (Inner db) =
    Inner <| dictSubtract da db


dictSubtract : Dict comparable v -> Dict comparable a -> Dict comparable v
dictSubtract da db =
    Dict.merge
        (\k a dict -> Dict.insert k a dict)
        (always <| always <| always identity)
        (always <| always identity)
        da
        db
        Dict.empty
