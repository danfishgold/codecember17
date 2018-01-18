module Hebrew.Verb exposing (..)


type alias Verb =
    { root : List String
    , conjugation : Conjugation
    , tense : Tense
    , person : Person
    , sex : Sex
    , quantity : Quantity
    }


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


type Effect
    = AConj Conjugation
    | ATense Tense
    | APerson Person
    | ASex Sex
    | AQuantity Quantity


effectTitle : Effect -> String
effectTitle effect =
    case effect of
        AConj Paal ->
            "פעל"

        AConj Nifal ->
            "נפעל"

        ATense Past ->
            "עבר"

        ATense Present ->
            "הווה"

        ATense Future ->
            "עתיד"

        ATense Imperative ->
            "ציווי"

        APerson First ->
            "גוף ראשון"

        APerson Second ->
            "גוף שני"

        APerson Third ->
            "גוף שלישי"

        ASex Male ->
            "זכר"

        ASex Female ->
            "נקבה"

        AQuantity Singular ->
            "יחיד"

        AQuantity Plural ->
            "רבים"


apply : Effect -> Verb -> Verb
apply effect verb =
    case effect of
        AConj conj ->
            { verb | conjugation = conj }

        ATense tense ->
            { verb | tense = tense }

        APerson person ->
            { verb | person = person }

        ASex sex ->
            { verb | sex = sex }

        AQuantity quantity ->
            { verb | quantity = quantity }


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
                        |> withFinalLetters

                Nifal ->
                    nifalToString p e l verb.tense verb.person verb.sex verb.quantity
                        |> withFinalLetters

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


withFinalLetters : String -> String
withFinalLetters word =
    let
        len =
            String.length word

        ( start, end ) =
            ( String.slice 0 -1 word, String.slice -1 len word )
    in
        case end of
            "כ" ->
                start ++ "ך"

            "מ" ->
                start ++ "ם"

            "נ" ->
                start ++ "ן"

            "פ" ->
                start ++ "ף"

            "צ" ->
                start ++ "ץ"

            _ ->
                word
