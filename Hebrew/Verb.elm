module Hebrew.Verb exposing (..)


type alias Verb =
    { root : List String
    , conjugation : Conjugation
    , tense : Tense
    , person : Person
    , sex : Sex
    , quantity : Quantity
    }


verb : List String -> Verb
verb root =
    { root = root
    , conjugation = Paal
    , tense = Past
    , person = First
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
                    ""

        _ ->
            "NOT SUPPORTED"


paalToString : String -> String -> String -> Tense -> Person -> Sex -> Quantity -> String
paalToString p e l tense person sex quantity =
    case ( tense, person, sex, quantity ) of
        ( Past, First, _, Singular ) ->
            p ++ e ++ l ++ "תי"

        ( Past, First, _, Plural ) ->
            p ++ e ++ l ++ "נו"

        ( Future, First, _, Singular ) ->
            "א" ++ p ++ e ++ l

        ( Future, First, _, Plural ) ->
            "נ" ++ p ++ e ++ l

        ( Past, Second, _, Singular ) ->
            p ++ e ++ l ++ "ת"

        ( Past, Second, Male, Plural ) ->
            p ++ e ++ l ++ "תם"

        ( Past, Second, Female, Plural ) ->
            p ++ e ++ l ++ "תן"

        ( Future, Second, Male, Singular ) ->
            "ת" ++ p ++ e ++ l

        ( Future, Second, Female, Singular ) ->
            "ת" ++ p ++ e ++ l ++ "י"

        ( Future, Second, Male, Plural ) ->
            "ת" ++ p ++ e ++ l ++ "ו"

        ( Future, Second, Female, Plural ) ->
            "ת" ++ p ++ e ++ l ++ "נה"

        ( Past, Third, Male, Singular ) ->
            p ++ e ++ l

        ( Past, Third, Female, Singular ) ->
            p ++ e ++ l ++ "ה"

        ( Past, Third, _, Plural ) ->
            p ++ e ++ l ++ "ו"

        ( Future, Third, Male, Singular ) ->
            "י" ++ p ++ e ++ l

        ( Future, Third, Female, Singular ) ->
            "ת" ++ p ++ e ++ l

        ( Future, Third, Male, Plural ) ->
            "י" ++ p ++ e ++ l ++ "ו"

        ( Future, Third, Female, Plural ) ->
            "ת" ++ p ++ e ++ l ++ "נה"

        ( Present, _, Male, Singular ) ->
            p ++ "ו" ++ e ++ l

        ( Present, _, Female, Singular ) ->
            p ++ "ו" ++ e ++ l ++ "ת"

        ( Present, _, Male, Plural ) ->
            p ++ "ו" ++ e ++ l ++ "ים"

        ( Present, _, Female, Plural ) ->
            p ++ "ו" ++ e ++ l ++ "ות"

        ( Imperative, _, Male, Singular ) ->
            p ++ e ++ l

        ( Imperative, _, Female, Singular ) ->
            p ++ e ++ l ++ "י"

        ( Imperative, _, Male, Plural ) ->
            p ++ e ++ l ++ "ו"

        ( Imperative, _, Female, Plural ) ->
            p ++ e ++ l ++ "נה"


nifalToString : String -> String -> String -> Tense -> Person -> Sex -> Quantity -> String
nifalToString p e l tense person sex quantity =
    case ( tense, person, sex, quantity ) of
        ( Past, First, _, Singular ) ->
            "נ" ++ p ++ e ++ l ++ "תי"

        ( Past, First, _, Plural ) ->
            "נ" ++ p ++ e ++ l ++ "נו"

        ( Future, First, _, Singular ) ->
            "א" ++ p ++ e ++ l

        ( Future, First, _, Plural ) ->
            "נ" ++ p ++ e ++ l

        ( Past, Second, _, Singular ) ->
            "נ" ++ p ++ e ++ l ++ "ת"

        ( Past, Second, Male, Plural ) ->
            "נ" ++ p ++ e ++ l ++ "תם"

        ( Past, Second, Female, Plural ) ->
            "נ" ++ p ++ e ++ l ++ "תן"

        ( Future, Second, Male, Singular ) ->
            "ת" ++ p ++ e ++ l

        ( Future, Second, Female, Singular ) ->
            "ת" ++ p ++ e ++ l ++ "י"

        ( Future, Second, Male, Plural ) ->
            "ת" ++ p ++ e ++ l ++ "ו"

        ( Future, Second, Female, Plural ) ->
            "ת" ++ p ++ e ++ l ++ "נה"

        ( Past, Third, Male, Singular ) ->
            "נ" ++ p ++ e ++ l

        ( Past, Third, Female, Singular ) ->
            "נ" ++ p ++ e ++ l ++ "ה"

        ( Past, Third, _, Plural ) ->
            "נ" ++ p ++ e ++ l ++ "ו"

        ( Future, Third, Male, Singular ) ->
            "י" ++ p ++ e ++ l

        ( Future, Third, Female, Singular ) ->
            "ת" ++ p ++ e ++ l

        ( Future, Third, Male, Plural ) ->
            "י" ++ p ++ e ++ l ++ "ו"

        ( Future, Third, Female, Plural ) ->
            "ת" ++ p ++ e ++ l ++ "נה"

        ( Present, _, Male, Singular ) ->
            "נ" ++ p ++ e ++ l

        ( Present, _, Female, Singular ) ->
            "נ" ++ p ++ e ++ l ++ "ת"

        ( Present, _, Male, Plural ) ->
            "נ" ++ p ++ e ++ l ++ "ים"

        ( Present, _, Female, Plural ) ->
            "נ" ++ p ++ e ++ l ++ "ות"

        ( Imperative, _, Male, Singular ) ->
            p ++ e ++ l

        ( Imperative, _, Female, Singular ) ->
            p ++ e ++ l ++ "י"

        ( Imperative, _, Male, Plural ) ->
            p ++ e ++ l ++ "ו"

        ( Imperative, _, Female, Plural ) ->
            p ++ e ++ l ++ "נה"


type Conjugation
    = Paal
    | Nifal


type Tense
    = Past
    | Present
    | Future
    | Imperative


type Person
    = First
    | Second
    | Third


type Sex
    = Male
    | Female


type Quantity
    = Singular
    | Plural
