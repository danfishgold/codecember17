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


setConstructState : ConstructState -> Noun -> Noun
setConstructState state noun =
    { noun | constructState = state }


changeConstructState : Noun -> Noun
changeConstructState noun =
    case noun.constructState of
        Possessor ->
            setConstructState Possessed noun

        Possessed ->
            setConstructState Possessor noun


setDefinite : Bool -> { np | isDefinite : Bool } -> { np | isDefinite : Bool }
setDefinite isDefinite np =
    { np | isDefinite = isDefinite }


changeDefinite : { np | isDefinite : Bool } -> { np | isDefinite : Bool }
changeDefinite np =
    setDefinite (not np.isDefinite) np


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
        |> mapLast (setDefinite construct.isDefinite >> List.singleton)


constructToString : Construct -> String
constructToString construct =
    construct.nouns
        |> List.map (setConstructState Possessed >> setDefinite False)
        |> mapLast (setConstructState Possessor >> setDefinite construct.isDefinite >> List.singleton)
        |> List.map toString
        |> String.join " "


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


makeString : Form -> ConstructState -> Quantity -> String -> String
makeString form constructState quantity ktl =
    case ( form, constructState, quantity ) of
        ( Katal, _, Singular ) ->
            ktl

        ( Katal, Possessor, Plural ) ->
            ktl ++ "ים"

        ( Katal, Possessed, Plural ) ->
            ktl ++ "י"

        ( Miktal, _, Singular ) ->
            "מ" ++ ktl

        ( Miktal, Possessor, Plural ) ->
            "מ" ++ ktl ++ "ים"

        ( Miktal, Possessed, Plural ) ->
            "מ" ++ ktl ++ "י"

        ( Katelet, _, Singular ) ->
            ktl ++ "ת"

        ( Katelet, _, Plural ) ->
            ktl ++ "ות"

        ( Miktala, Possessor, Singular ) ->
            "מ" ++ ktl ++ "ה"

        ( Miktala, Possessed, Singular ) ->
            "מ" ++ ktl ++ "ת"

        ( Miktala, _, Plural ) ->
            "מ" ++ ktl ++ "ות"

        ( Katlia, Possessor, Singular ) ->
            ktl ++ "יה"

        ( Katlia, Possessed, Singular ) ->
            ktl ++ "יית"

        ( Katlia, _, Plural ) ->
            ktl ++ "יות"


toString : Noun -> String
toString noun =
    case ktl noun.root of
        Nothing ->
            "NOT SUPPORTED"

        Just ktl_ ->
            let
                string =
                    makeString noun.form noun.constructState noun.quantity ktl_
            in
                if noun.constructState == Possessor && noun.isDefinite then
                    "ה" ++ string
                else
                    string
