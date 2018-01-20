module Hebrew.Noun exposing (..)

import Util exposing (between, (?>), mapLast)


type ConstructState
    = Possessed
    | Possessor


type Form
    = Katal
    | Miktal
    | Katelet
    | Miktala
    | Katlia


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


toString : Noun -> String
toString noun =
    case ktl noun.root of
        Nothing ->
            "NOT SUPPORTED"

        Just ktl_ ->
            case ( noun.form, noun.constructState ) of
                ( Katal, _ ) ->
                    ktl_

                ( Miktal, _ ) ->
                    "מ" ++ ktl_

                ( Katelet, _ ) ->
                    ktl_ ++ "ת"

                ( Miktala, Possessor ) ->
                    "מ" ++ ktl_ ++ "ה"

                ( Miktala, Possessed ) ->
                    "מ" ++ ktl_ ++ "ת"

                ( Katlia, Possessor ) ->
                    ktl_ ++ "יה"

                ( Katlia, Possessed ) ->
                    ktl_ ++ "יית"
