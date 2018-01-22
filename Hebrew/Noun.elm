module Hebrew.Noun exposing (..)

import Util exposing (between, (?>), mapLast)
import Hebrew.Base as Base exposing (Quantity(..))


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
    , quantity : Quantity
    }


type alias Construct =
    { nouns : List Noun
    , isDefinite : Bool
    }


noun : List String -> Noun
noun root =
    { root = root
    , constructState = Possessor
    , isDefinite = False
    , form = Katal
    , quantity = Singular
    }


setForm : Form -> Noun -> Noun
setForm form noun =
    { noun | form = form }


setQuantity : Quantity -> Noun -> Noun
setQuantity quantity noun =
    { noun | quantity = quantity }


setConstructState : ConstructState -> Noun -> Noun
setConstructState state noun =
    { noun | constructState = state }


formTitle : Form -> String
formTitle form =
    case form of
        Katal ->
            "קטל"

        Miktal ->
            "מקטל"

        Katelet ->
            "קטלת"

        Miktala ->
            "מקטלה"

        Katlia ->
            "קטליה"


constructStateTitle : ConstructState -> String
constructStateTitle state =
    case state of
        Possessor ->
            "סומך"

        Possessed ->
            "נסמך"


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
            case ( noun.form, noun.constructState, noun.quantity ) of
                ( Katal, _, Singular ) ->
                    ktl_

                ( Katal, Possessor, Plural ) ->
                    ktl_ ++ "ים"

                ( Katal, Possessed, Plural ) ->
                    ktl_ ++ "י"

                ( Miktal, _, Singular ) ->
                    "מ" ++ ktl_

                ( Miktal, Possessor, Plural ) ->
                    "מ" ++ ktl_ ++ "ים"

                ( Miktal, Possessed, Plural ) ->
                    "מ" ++ ktl_ ++ "י"

                ( Katelet, _, Singular ) ->
                    ktl_ ++ "ת"

                ( Katelet, _, Plural ) ->
                    ktl_ ++ "ות"

                ( Miktala, Possessor, Singular ) ->
                    "מ" ++ ktl_ ++ "ה"

                ( Miktala, Possessed, Singular ) ->
                    "מ" ++ ktl_ ++ "ת"

                ( Miktala, _, Plural ) ->
                    "מ" ++ ktl_ ++ "ות"

                ( Katlia, Possessor, Singular ) ->
                    ktl_ ++ "יה"

                ( Katlia, Possessed, Singular ) ->
                    ktl_ ++ "יית"

                ( Katlia, _, Plural ) ->
                    ktl_ ++ "יות"
