module Hebrew.Noun exposing
    ( Construct
    , ConstructState
    , Form(..)
    , Noun
    , changeConstructState
    , changeDefinite
    , construct
    , constructStateTitle
    , constructToString
    , formTitle
    , noun
    , setForm
    , split
    , toString
    )

import Hebrew.Base exposing (Quantity(..))
import Util exposing (mapLast)


type ConstructState
    = Possessed
    | Possessor


type Form
    = Katal
    | Miktal
    | Katelet
    | Miktala
    | Katlia
    | Taktil


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


type alias Splits =
    { ktl : String, kt : String, l : String }


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

        Taktil ->
            "תקטיל"


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


splits : List String -> Maybe Splits
splits root =
    case root of
        [ k, t, l ] ->
            Just { ktl = k ++ t ++ l, kt = k ++ t, l = l }

        [ k, a, t, l ] ->
            Just { ktl = k ++ a ++ t ++ l, kt = k ++ a ++ t, l = l }

        _ ->
            Nothing


makeString : Form -> ConstructState -> Quantity -> Splits -> String
makeString form constructState quantity { ktl, kt, l } =
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

        ( Taktil, _, Singular ) ->
            "ת" ++ kt ++ "י" ++ l

        ( Taktil, Possessor, Plural ) ->
            "ת" ++ kt ++ "י" ++ l ++ "ים"

        ( Taktil, Possessed, Plural ) ->
            "ת" ++ kt ++ "י" ++ l ++ "י"


toString : Noun -> String
toString noun =
    case splits noun.root of
        Nothing ->
            "NOT SUPPORTED"

        Just splits ->
            let
                string =
                    makeString noun.form noun.constructState noun.quantity splits
            in
            if noun.constructState == Possessor && noun.isDefinite then
                "ה" ++ string

            else
                string
