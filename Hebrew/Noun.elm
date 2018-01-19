module Hebrew.Noun exposing (..)

import Util exposing (between, (?>), mapLast)


type alias Form =
    ConstructState -> List String -> Maybe String


type ConstructState
    = Possessed
    | Possessor


type alias Noun =
    { root : List String
    , constructState : ConstructState
    , isDefinite : Bool
    , form : Form
    }


type alias Construct =
    { nouns : List Noun
    , isDefinite : Bool
    }


construct : List Noun -> Construct
construct nouns =
    { nouns = nouns
    , isDefinite = List.any .isDefinite nouns
    }


split : Construct -> List Noun
split construct =
    construct.nouns
        |> List.map (\n -> { n | isDefinite = False })
        |> mapLast (\n -> [ { n | isDefinite = construct.isDefinite } ])


ktl : List String -> Maybe String
ktl root =
    let
        len =
            List.length root
    in
        if 3 <= len && len <= 4 then
            Just <| String.join "" root
        else
            Nothing


katal : Form
katal state root =
    ktl root


miktal : Form
miktal state root =
    ktl root ?> \ktl -> "מ" ++ ktl


katelet : Form
katelet state root =
    ktl root ?> \ktl -> ktl ++ "ת"


miktala : Form
miktala state root =
    case state of
        Possessor ->
            ktl root ?> \ktl -> "מ" ++ ktl ++ "ה"

        Possessed ->
            ktl root ?> \ktl -> "מ" ++ ktl ++ "ת"


katlia : Form
katlia state root =
    case state of
        Possessor ->
            ktl root ?> \ktl -> ktl ++ "יה"

        Possessed ->
            ktl root ?> \ktl -> ktl ++ "יית"
