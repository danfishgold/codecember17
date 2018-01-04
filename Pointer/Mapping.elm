module Pointer.Mapping exposing (..)

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


map : (Id -> a -> b) -> Mapping a -> List b
map fn (Inner dict) =
    Dict.toList dict
        |> List.map
            (\( idString, value ) ->
                fn (Pointer.Id.fromString idString) value
            )


mutualMap3 : (a -> b -> c -> d) -> Mapping a -> Mapping b -> Mapping c -> Mapping d
mutualMap3 fn pa pb pc =
    mutualMap2
        (\( a, b ) c -> fn a b c)
        (mutualMap2 (,) pa pb)
        pc


mutualMap2 : (a -> b -> c) -> Mapping a -> Mapping b -> Mapping c
mutualMap2 fn (Inner pa) (Inner pb) =
    Dict.merge
        (\k a dict -> dict)
        (\k a b dict -> Dict.insert k (fn a b) dict)
        (\k b dict -> dict)
        pa
        pb
        Dict.empty
        |> fromDict


subtract : Mapping a -> Mapping b -> Mapping a
subtract (Inner pa) (Inner pb) =
    Dict.merge
        (\k a dict -> Dict.insert k a dict)
        (\k a b dict -> dict)
        (\k b dict -> dict)
        pa
        pb
        Dict.empty
        |> fromDict


mapValues : (a -> b) -> Mapping a -> List b
mapValues fn pointers =
    toList pointers |> List.map fn


empty : Mapping a
empty =
    Inner (Dict.empty)


foldl : (Id -> a -> b -> b) -> b -> Mapping a -> b
foldl folder initial (Inner dict) =
    let
        newFolder stringId a b =
            folder (Pointer.Id.fromString stringId) a b
    in
        Dict.foldl newFolder initial dict
