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


type alias Splits =
    { pal : String
    , p : String
    , pa : String
    , al : String
    , l : String
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
    maybeWord verb |> Maybe.withDefault "NOT SUPPORTED"


maybeWord : Verb -> Maybe String
maybeWord verb =
    case splits verb.root of
        Just spl ->
            case verb.conjugation of
                Paal ->
                    Just <| paalToString spl verb.tense verb.person verb.sex verb.quantity

                Nifal ->
                    Just <| nifalToString spl verb.tense verb.person verb.sex verb.quantity


                Hitpael ->
                    Just <| hitpaelToString spl verb.tense verb.person verb.sex verb.quantity

        Nothing ->
            Nothing


splits : List String -> Maybe Splits
splits root =
    case root of
        [ p, e, l ] ->
            Just <|
                { pal = p ++ e ++ l
                , p = p
                , pa = p ++ e
                , al = e ++ l
                , l = l
                }

        [ p, a, e, l ] ->
            Just <|
                { pal = p ++ a ++ e ++ l
                , p = p
                , pa = p ++ a ++ e
                , al = a ++ e ++ l
                , l = l
                }

        _ ->
            Nothing


paalToString : Splits -> Tense -> Person -> Sex -> Quantity -> String
paalToString { pal, p, al } tense person sex quantity =
    let
        poel =
            p ++ "ו" ++ al
    in
        case tense of
            Past ->
                case ( person, sex, quantity ) of
                    ( First, _, Singular ) ->
                        pal ++ "תי"

                    ( First, _, Plural ) ->
                        pal ++ "נו"

                    ( Second, _, Singular ) ->
                        pal ++ "ת"

                    ( Second, Male, Plural ) ->
                        pal ++ "תם"

                    ( Second, Female, Plural ) ->
                        pal ++ "תן"

                    ( Third, Male, Singular ) ->
                        pal

                    ( Third, Female, Singular ) ->
                        pal ++ "ה"

                    ( Third, _, Plural ) ->
                        pal ++ "ו"

            Present ->
                case ( sex, quantity ) of
                    ( Male, Singular ) ->
                        poel

                    ( Female, Singular ) ->
                        poel ++ "ת"

                    ( Male, Plural ) ->
                        poel ++ "ים"

                    ( Female, Plural ) ->
                        poel ++ "ות"

            Future ->
                case ( person, sex, quantity ) of
                    ( First, _, Singular ) ->
                        "א" ++ pal

                    ( First, _, Plural ) ->
                        "נ" ++ pal

                    ( Second, Male, Singular ) ->
                        "ת" ++ pal

                    ( Second, Female, Singular ) ->
                        "ת" ++ pal ++ "י"

                    ( Second, Male, Plural ) ->
                        "ת" ++ pal ++ "ו"

                    ( Second, Female, Plural ) ->
                        "ת" ++ pal ++ "נה"

                    ( Third, Male, Singular ) ->
                        "י" ++ pal

                    ( Third, Female, Singular ) ->
                        "ת" ++ pal

                    ( Third, Male, Plural ) ->
                        "י" ++ pal ++ "ו"

                    ( Third, Female, Plural ) ->
                        "ת" ++ pal ++ "נה"

            Imperative ->
                case ( sex, quantity ) of
                    ( Male, Singular ) ->
                        pal

                    ( Female, Singular ) ->
                        pal ++ "י"

                    ( Male, Plural ) ->
                        pal ++ "ו"

                    ( Female, Plural ) ->
                        pal ++ "נה"


nifalToString : Splits -> Tense -> Person -> Sex -> Quantity -> String
nifalToString { pal } tense person sex quantity =
    case tense of
        Past ->
            case ( person, sex, quantity ) of
                ( First, _, Singular ) ->
                    "נ" ++ pal ++ "תי"

                ( First, _, Plural ) ->
                    "נ" ++ pal ++ "נו"

                ( Second, _, Singular ) ->
                    "נ" ++ pal ++ "ת"

                ( Second, Male, Plural ) ->
                    "נ" ++ pal ++ "תם"

                ( Second, Female, Plural ) ->
                    "נ" ++ pal ++ "תן"

                ( Third, Male, Singular ) ->
                    "נ" ++ pal

                ( Third, Female, Singular ) ->
                    "נ" ++ pal ++ "ה"

                ( Third, _, Plural ) ->
                    "נ" ++ pal ++ "ו"

        Present ->
            case ( sex, quantity ) of
                ( Male, Singular ) ->
                    "נ" ++ pal

                ( Female, Singular ) ->
                    "נ" ++ pal ++ "ת"

                ( Male, Plural ) ->
                    "נ" ++ pal ++ "ים"

                ( Female, Plural ) ->
                    "נ" ++ pal ++ "ות"

        Future ->
            case ( person, sex, quantity ) of
                ( First, _, Singular ) ->
                    "א" ++ pal

                ( First, _, Plural ) ->
                    "נ" ++ pal

                ( Second, Male, Singular ) ->
                    "ת" ++ pal

                ( Second, Female, Singular ) ->
                    "ת" ++ pal ++ "י"

                ( Second, Male, Plural ) ->
                    "ת" ++ pal ++ "ו"

                ( Second, Female, Plural ) ->
                    "ת" ++ pal ++ "נה"

                ( Third, Male, Singular ) ->
                    "י" ++ pal

                ( Third, Female, Singular ) ->
                    "ת" ++ pal

                ( Third, Male, Plural ) ->
                    "י" ++ pal ++ "ו"

                ( Third, Female, Plural ) ->
                    "ת" ++ pal ++ "נה"

        Imperative ->
            case ( sex, quantity ) of
                ( Male, Singular ) ->
                    pal

                ( Female, Singular ) ->
                    pal ++ "י"

                ( Male, Plural ) ->
                    pal ++ "ו"

                ( Female, Plural ) ->
                    pal ++ "נה"


hitpaelToString : Splits -> Tense -> Person -> Sex -> Quantity -> String
hitpaelToString { p, al } tense person sex quantity =
    let
        ( t, p_ ) =
            hitpaelHelper p

        tpael =
            t ++ p_ ++ al
    in
        case tense of
            Past ->
                case ( person, sex, quantity ) of
                    ( First, _, Singular ) ->
                        "ה" ++ tpael ++ "תי"

                    ( First, _, Plural ) ->
                        "ה" ++ tpael ++ "נו"

                    ( Second, _, Singular ) ->
                        "ה" ++ tpael ++ "ת"

                    ( Second, Male, Plural ) ->
                        "ה" ++ tpael ++ "תם"

                    ( Second, Female, Plural ) ->
                        "ה" ++ tpael ++ "תן"

                    ( Third, Male, Singular ) ->
                        "ה" ++ tpael

                    ( Third, Female, Singular ) ->
                        "ה" ++ tpael ++ "ה"

                    ( Third, _, Plural ) ->
                        "ה" ++ tpael ++ "ו"

            Present ->
                case ( sex, quantity ) of
                    ( Male, Singular ) ->
                        "מ" ++ tpael

                    ( Female, Singular ) ->
                        "מ" ++ tpael ++ "ת"

                    ( Male, Plural ) ->
                        "מ" ++ tpael ++ "ים"

                    ( Female, Plural ) ->
                        "מ" ++ tpael ++ "ות"

            Future ->
                case ( person, sex, quantity ) of
                    ( First, _, Singular ) ->
                        "א" ++ tpael

                    ( First, _, Plural ) ->
                        "נ" ++ tpael

                    ( Second, Male, Singular ) ->
                        "ת" ++ tpael

                    ( Second, Female, Singular ) ->
                        "ת" ++ tpael ++ "י"

                    ( Second, Male, Plural ) ->
                        "ת" ++ tpael ++ "ו"

                    ( Second, Female, Plural ) ->
                        "ת" ++ tpael ++ "נה"

                    ( Third, Male, Singular ) ->
                        "י" ++ tpael

                    ( Third, Female, Singular ) ->
                        "ת" ++ tpael

                    ( Third, Male, Plural ) ->
                        "י" ++ tpael ++ "ו"

                    ( Third, Female, Plural ) ->
                        "ת" ++ tpael ++ "נה"

            Imperative ->
                case ( sex, quantity ) of
                    ( Male, Singular ) ->
                        "ה" ++ tpael

                    ( Female, Singular ) ->
                        "ה" ++ tpael ++ "י"

                    ( Male, Plural ) ->
                        "ה" ++ tpael ++ "ו"

                    ( Female, Plural ) ->
                        "ה" ++ tpael ++ "נה"


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
