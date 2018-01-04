module Pointers exposing (..)

import Pointer exposing (Pointer, Identifier, idToString)
import Dict exposing (Dict)


type Pointers a
    = Pointers (Dict String a)


fromDict : Dict String a -> Pointers a
fromDict dict =
    Pointers dict


add : Identifier -> a -> Pointers a -> Pointers a
add identifier a (Pointers dict) =
    Pointers (Dict.insert (idToString identifier) a dict)


remove : Identifier -> Pointers a -> Pointers a
remove identifier (Pointers dict) =
    Pointers (Dict.remove (idToString identifier) dict)


extract : List Identifier -> Pointers a -> ( Pointers a, List a )
extract identifiers (Pointers dict) =
    let
        ids =
            List.map idToString identifiers

        ( extracted, remaining ) =
            Dict.partition (\id _ -> List.member id ids) dict
    in
        ( Pointers remaining, Dict.values extracted )


toList : Pointers a -> List a
toList (Pointers dict) =
    Dict.values dict


map : (Identifier -> a -> b) -> Pointers a -> List b
map fn (Pointers dict) =
    Dict.toList dict
        |> List.map
            (\( identifierString, value ) ->
                fn (Pointer.identifier identifierString) value
            )


mutualMap3 : (a -> b -> c -> d) -> Pointers a -> Pointers b -> Pointers c -> Pointers d
mutualMap3 fn pa pb pc =
    mutualMap2
        (\( a, b ) c -> fn a b c)
        (mutualMap2 (,) pa pb)
        pc


mutualMap2 : (a -> b -> c) -> Pointers a -> Pointers b -> Pointers c
mutualMap2 fn (Pointers pa) (Pointers pb) =
    Dict.merge
        (\k a dict -> dict)
        (\k a b dict -> Dict.insert k (fn a b) dict)
        (\k b dict -> dict)
        pa
        pb
        Dict.empty
        |> fromDict


subtract : Pointers a -> Pointers b -> Pointers a
subtract (Pointers pa) (Pointers pb) =
    Dict.merge
        (\k a dict -> Dict.insert k a dict)
        (\k a b dict -> dict)
        (\k b dict -> dict)
        pa
        pb
        Dict.empty
        |> fromDict


mapValues : (a -> b) -> Pointers a -> List b
mapValues fn pointers =
    toList pointers |> List.map fn


empty : Pointers a
empty =
    Pointers (Dict.empty)


foldl : (Identifier -> a -> b -> b) -> b -> Pointers a -> b
foldl folder initial (Pointers dict) =
    let
        newFolder stringIdentifier a b =
            folder (Pointer.identifier stringIdentifier) a b
    in
        Dict.foldl newFolder initial dict
