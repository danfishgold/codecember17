module Hebrew.Verb exposing (..)

import Hebrew.Base as Base
    exposing
        ( Tense(..)
        , Person(..)
        , Sex(..)
        , Quantity(..)
        )


type Conjugation
    = Paal
    | Nifal
    | Hitpael


type alias Verb =
    { root : List String
    , conjugation : Conjugation
    , tense : Tense
    , person : Person
    , sex : Sex
    , quantity : Quantity
    }


conjugationTitle : Conjugation -> String
conjugationTitle conj =
    case conj of
        Paal ->
            "פעל"

        Nifal ->
            "נפעל"

        Hitpael ->
            "התפעל"


setConjugation : Conjugation -> Verb -> Verb
setConjugation conj verb =
    { verb | conjugation = conj }


verb : List String -> Verb
verb root =
    { root = root
    , conjugation = Paal
    , tense = Past
    , person = Third
    , sex = Male
    , quantity = Singular
    }


toString : Verb -> String
toString verb =
    case verb.root of
        [ p, e, l ] ->
            case verb.conjugation of
                Paal ->
                    paalToString p e l verb.tense verb.person verb.sex verb.quantity

                Nifal ->
                    nifalToString p e l verb.tense verb.person verb.sex verb.quantity

                Hitpael ->
                    hitpaelToString p e l verb.tense verb.person verb.sex verb.quantity

        _ ->
            "NOT SUPPORTED"


paalToString : String -> String -> String -> Tense -> Person -> Sex -> Quantity -> String
paalToString p e l tense person sex quantity =
    case tense of
        Past ->
            case ( person, sex, quantity ) of
                ( First, _, Singular ) ->
                    p ++ e ++ l ++ "תי"

                ( First, _, Plural ) ->
                    p ++ e ++ l ++ "נו"

                ( Second, _, Singular ) ->
                    p ++ e ++ l ++ "ת"

                ( Second, Male, Plural ) ->
                    p ++ e ++ l ++ "תם"

                ( Second, Female, Plural ) ->
                    p ++ e ++ l ++ "תן"

                ( Third, Male, Singular ) ->
                    p ++ e ++ l

                ( Third, Female, Singular ) ->
                    p ++ e ++ l ++ "ה"

                ( Third, _, Plural ) ->
                    p ++ e ++ l ++ "ו"

        Present ->
            case ( sex, quantity ) of
                ( Male, Singular ) ->
                    p ++ "ו" ++ e ++ l

                ( Female, Singular ) ->
                    p ++ "ו" ++ e ++ l ++ "ת"

                ( Male, Plural ) ->
                    p ++ "ו" ++ e ++ l ++ "ים"

                ( Female, Plural ) ->
                    p ++ "ו" ++ e ++ l ++ "ות"

        Future ->
            case ( person, sex, quantity ) of
                ( First, _, Singular ) ->
                    "א" ++ p ++ e ++ l

                ( First, _, Plural ) ->
                    "נ" ++ p ++ e ++ l

                ( Second, Male, Singular ) ->
                    "ת" ++ p ++ e ++ l

                ( Second, Female, Singular ) ->
                    "ת" ++ p ++ e ++ l ++ "י"

                ( Second, Male, Plural ) ->
                    "ת" ++ p ++ e ++ l ++ "ו"

                ( Second, Female, Plural ) ->
                    "ת" ++ p ++ e ++ l ++ "נה"

                ( Third, Male, Singular ) ->
                    "י" ++ p ++ e ++ l

                ( Third, Female, Singular ) ->
                    "ת" ++ p ++ e ++ l

                ( Third, Male, Plural ) ->
                    "י" ++ p ++ e ++ l ++ "ו"

                ( Third, Female, Plural ) ->
                    "ת" ++ p ++ e ++ l ++ "נה"

        Imperative ->
            case ( sex, quantity ) of
                ( Male, Singular ) ->
                    p ++ e ++ l

                ( Female, Singular ) ->
                    p ++ e ++ l ++ "י"

                ( Male, Plural ) ->
                    p ++ e ++ l ++ "ו"

                ( Female, Plural ) ->
                    p ++ e ++ l ++ "נה"


nifalToString : String -> String -> String -> Tense -> Person -> Sex -> Quantity -> String
nifalToString p e l tense person sex quantity =
    case tense of
        Past ->
            case ( person, sex, quantity ) of
                ( First, _, Singular ) ->
                    "נ" ++ p ++ e ++ l ++ "תי"

                ( First, _, Plural ) ->
                    "נ" ++ p ++ e ++ l ++ "נו"

                ( Second, _, Singular ) ->
                    "נ" ++ p ++ e ++ l ++ "ת"

                ( Second, Male, Plural ) ->
                    "נ" ++ p ++ e ++ l ++ "תם"

                ( Second, Female, Plural ) ->
                    "נ" ++ p ++ e ++ l ++ "תן"

                ( Third, Male, Singular ) ->
                    "נ" ++ p ++ e ++ l

                ( Third, Female, Singular ) ->
                    "נ" ++ p ++ e ++ l ++ "ה"

                ( Third, _, Plural ) ->
                    "נ" ++ p ++ e ++ l ++ "ו"

        Present ->
            case ( sex, quantity ) of
                ( Male, Singular ) ->
                    "נ" ++ p ++ e ++ l

                ( Female, Singular ) ->
                    "נ" ++ p ++ e ++ l ++ "ת"

                ( Male, Plural ) ->
                    "נ" ++ p ++ e ++ l ++ "ים"

                ( Female, Plural ) ->
                    "נ" ++ p ++ e ++ l ++ "ות"

        Future ->
            case ( person, sex, quantity ) of
                ( First, _, Singular ) ->
                    "א" ++ p ++ e ++ l

                ( First, _, Plural ) ->
                    "נ" ++ p ++ e ++ l

                ( Second, Male, Singular ) ->
                    "ת" ++ p ++ e ++ l

                ( Second, Female, Singular ) ->
                    "ת" ++ p ++ e ++ l ++ "י"

                ( Second, Male, Plural ) ->
                    "ת" ++ p ++ e ++ l ++ "ו"

                ( Second, Female, Plural ) ->
                    "ת" ++ p ++ e ++ l ++ "נה"

                ( Third, Male, Singular ) ->
                    "י" ++ p ++ e ++ l

                ( Third, Female, Singular ) ->
                    "ת" ++ p ++ e ++ l

                ( Third, Male, Plural ) ->
                    "י" ++ p ++ e ++ l ++ "ו"

                ( Third, Female, Plural ) ->
                    "ת" ++ p ++ e ++ l ++ "נה"

        Imperative ->
            case ( sex, quantity ) of
                ( Male, Singular ) ->
                    p ++ e ++ l

                ( Female, Singular ) ->
                    p ++ e ++ l ++ "י"

                ( Male, Plural ) ->
                    p ++ e ++ l ++ "ו"

                ( Female, Plural ) ->
                    p ++ e ++ l ++ "נה"


hitpaelToString : String -> String -> String -> Tense -> Person -> Sex -> Quantity -> String
hitpaelToString p e l tense person sex quantity =
    let
        ( t, p_ ) =
            hitpaelHelper p
    in
        case tense of
            Past ->
                case ( person, sex, quantity ) of
                    ( First, _, Singular ) ->
                        "ה" ++ t ++ p_ ++ e ++ l ++ "תי"

                    ( First, _, Plural ) ->
                        "ה" ++ t ++ p_ ++ e ++ l ++ "נו"

                    ( Second, _, Singular ) ->
                        "ה" ++ t ++ p_ ++ e ++ l ++ "ת"

                    ( Second, Male, Plural ) ->
                        "ה" ++ t ++ p_ ++ e ++ l ++ "תם"

                    ( Second, Female, Plural ) ->
                        "ה" ++ t ++ p_ ++ e ++ l ++ "תן"

                    ( Third, Male, Singular ) ->
                        "ה" ++ t ++ p_ ++ e ++ l

                    ( Third, Female, Singular ) ->
                        "ה" ++ t ++ p_ ++ e ++ l ++ "ה"

                    ( Third, _, Plural ) ->
                        "ה" ++ t ++ p_ ++ e ++ l ++ "ו"

            Present ->
                case ( sex, quantity ) of
                    ( Male, Singular ) ->
                        "מ" ++ t ++ p_ ++ e ++ l

                    ( Female, Singular ) ->
                        "מ" ++ t ++ p_ ++ e ++ l ++ "ת"

                    ( Male, Plural ) ->
                        "מ" ++ t ++ p_ ++ e ++ l ++ "ים"

                    ( Female, Plural ) ->
                        "מ" ++ t ++ p_ ++ e ++ l ++ "ות"

            Future ->
                case ( person, sex, quantity ) of
                    ( First, _, Singular ) ->
                        "א" ++ t ++ p_ ++ e ++ l

                    ( First, _, Plural ) ->
                        "נ" ++ t ++ p_ ++ e ++ l

                    ( Second, Male, Singular ) ->
                        "ת" ++ t ++ p_ ++ e ++ l

                    ( Second, Female, Singular ) ->
                        "ת" ++ t ++ p_ ++ e ++ l ++ "י"

                    ( Second, Male, Plural ) ->
                        "ת" ++ t ++ p_ ++ e ++ l ++ "ו"

                    ( Second, Female, Plural ) ->
                        "ת" ++ t ++ p_ ++ e ++ l ++ "נה"

                    ( Third, Male, Singular ) ->
                        "י" ++ t ++ p_ ++ e ++ l

                    ( Third, Female, Singular ) ->
                        "ת" ++ t ++ p_ ++ e ++ l

                    ( Third, Male, Plural ) ->
                        "י" ++ t ++ p_ ++ e ++ l ++ "ו"

                    ( Third, Female, Plural ) ->
                        "ת" ++ t ++ p_ ++ e ++ l ++ "נה"

            Imperative ->
                case ( sex, quantity ) of
                    ( Male, Singular ) ->
                        "ה" ++ t ++ p_ ++ e ++ l

                    ( Female, Singular ) ->
                        "ה" ++ t ++ p_ ++ e ++ l ++ "י"

                    ( Male, Plural ) ->
                        "ה" ++ t ++ p_ ++ e ++ l ++ "ו"

                    ( Female, Plural ) ->
                        "ה" ++ t ++ p_ ++ e ++ l ++ "נה"


hitpaelHelper : String -> ( String, String )
hitpaelHelper p =
    if p == "ס" || p == "ש" then
        ( p, "ת" )
    else if p == "ז" then
        ( p, "ד" )
    else if p == "צ" then
        ( p, "ט" )
    else if p == "ת" then
        ( "", p )
    else if p == "ד" || p == "ט" then
        ( "", p )
    else
        ( "ת", p )
