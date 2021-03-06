module Hebrew.Verb exposing
    ( Conjugation(..)
    , Splits
    , Verb
    , conjugationTitle
    , setConjugation
    , toString
    , verb
    )

import Hebrew.Base
    exposing
        ( Person(..)
        , Quantity(..)
        , Sex(..)
        , Tense(..)
        )


type Conjugation
    = Paal
    | Nifal
    | Hifil
    | Hufal
    | Piel
    | Pual
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
            "קל"

        Nifal ->
            "נפעל"

        Hifil ->
            "הפעיל"

        Hufal ->
            "הופעל"

        Piel ->
            "פיעל"

        Pual ->
            "פועל"

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

                Hifil ->
                    Just <| hifilToString spl verb.tense verb.person verb.sex verb.quantity

                Hufal ->
                    Just <| hufalToString spl verb.tense verb.person verb.sex verb.quantity

                Piel ->
                    Just <| pielToString spl verb.tense verb.person verb.sex verb.quantity

                Pual ->
                    Just <| pualToString spl verb.tense verb.person verb.sex verb.quantity

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


beginningAndEnd : Tense -> Person -> Sex -> Quantity -> ( String, String )
beginningAndEnd tense person sex quantity =
    case tense of
        Past ->
            case ( person, sex, quantity ) of
                ( First, _, Singular ) ->
                    ( "", "תי" )

                ( First, _, Plural ) ->
                    ( "", "נו" )

                ( Second, _, Singular ) ->
                    ( "", "ת" )

                ( Second, Male, Plural ) ->
                    ( "", "תם" )

                ( Second, Female, Plural ) ->
                    ( "", "תן" )

                ( Third, Male, Singular ) ->
                    ( "", "" )

                ( Third, Female, Singular ) ->
                    ( "", "ה" )

                ( Third, _, Plural ) ->
                    ( "", "ו" )

        Present ->
            case ( sex, quantity ) of
                ( Male, Singular ) ->
                    ( "", "" )

                ( Female, Singular ) ->
                    ( "", "ת" )

                ( Male, Plural ) ->
                    ( "", "ים" )

                ( Female, Plural ) ->
                    ( "", "ות" )

        Future ->
            case ( person, sex, quantity ) of
                ( First, _, Singular ) ->
                    ( "א", "" )

                ( First, _, Plural ) ->
                    ( "נ", "" )

                ( Second, Male, Singular ) ->
                    ( "ת", "" )

                ( Second, Female, Singular ) ->
                    ( "ת", "י" )

                ( Second, Male, Plural ) ->
                    ( "ת", "ו" )

                ( Second, Female, Plural ) ->
                    ( "ת", "נה" )

                ( Third, Male, Singular ) ->
                    ( "י", "" )

                ( Third, Female, Singular ) ->
                    ( "ת", "" )

                ( Third, Male, Plural ) ->
                    ( "י", "ו" )

                ( Third, Female, Plural ) ->
                    ( "ת", "נה" )

        Imperative ->
            case ( sex, quantity ) of
                ( Male, Singular ) ->
                    ( "", "" )

                ( Female, Singular ) ->
                    ( "", "י" )

                ( Male, Plural ) ->
                    ( "", "ו" )

                ( Female, Plural ) ->
                    ( "", "נה" )


paalToString : Splits -> Tense -> Person -> Sex -> Quantity -> String
paalToString { pal, p, al } tense person sex quantity =
    let
        poel =
            p ++ "ו" ++ al

        ( beginning, end ) =
            beginningAndEnd tense person sex quantity
    in
    if tense == Present then
        beginning ++ poel ++ end

    else
        beginning ++ pal ++ end


nifalToString : Splits -> Tense -> Person -> Sex -> Quantity -> String
nifalToString { pal } tense person sex quantity =
    let
        ( beginning, end ) =
            beginningAndEnd tense person sex quantity
    in
    case tense of
        Past ->
            "נ" ++ pal ++ end

        Present ->
            "נ" ++ pal ++ end

        Future ->
            beginning ++ pal ++ end

        Imperative ->
            "ה" ++ pal ++ end


hifilToString : Splits -> Tense -> Person -> Sex -> Quantity -> String
hifilToString { pal, pa, l } tense person sex quantity =
    let
        pail =
            pa ++ "י" ++ l

        ( beginning, end ) =
            beginningAndEnd tense person sex quantity
    in
    case tense of
        Past ->
            if person == Third then
                "ה" ++ pail ++ end

            else
                "ה" ++ pal ++ end

        Present ->
            if ( sex, quantity ) == ( Female, Singular ) then
                "מ" ++ pail ++ "ה"

            else
                "מ" ++ pail ++ end

        Future ->
            beginning ++ pail ++ end

        Imperative ->
            case ( sex, quantity ) of
                ( Male, Singular ) ->
                    "ה" ++ pal ++ end

                ( Female, Singular ) ->
                    "ה" ++ pail ++ end

                ( Male, Plural ) ->
                    "ה" ++ pail ++ end

                ( Female, Plural ) ->
                    "ה" ++ pal ++ end


hufalToString : Splits -> Tense -> Person -> Sex -> Quantity -> String
hufalToString { pal } tense person sex quantity =
    let
        ( beginning, end ) =
            beginningAndEnd tense person sex quantity
    in
    case tense of
        Past ->
            "הו" ++ pal ++ end

        Present ->
            "מו" ++ pal ++ end

        Future ->
            beginning ++ "ו" ++ pal ++ end

        Imperative ->
            "אין ציווי לפועל סביל"


pielToString : Splits -> Tense -> Person -> Sex -> Quantity -> String
pielToString { p, al, pal } tense person sex quantity =
    let
        pial =
            p ++ "י" ++ al

        ( beginning, end ) =
            beginningAndEnd tense person sex quantity
    in
    case tense of
        Past ->
            beginning ++ pial ++ end

        Present ->
            "מ" ++ pal ++ end

        Future ->
            beginning ++ pal ++ end

        Imperative ->
            beginning ++ pal ++ end


pualToString : Splits -> Tense -> Person -> Sex -> Quantity -> String
pualToString { p, al } tense person sex quantity =
    let
        pual =
            p ++ "ו" ++ al

        ( beginning, end ) =
            beginningAndEnd tense person sex quantity
    in
    case tense of
        Past ->
            beginning ++ pual ++ end

        Present ->
            "מ" ++ pual ++ end

        Future ->
            beginning ++ pual ++ end

        Imperative ->
            "אין ציווי לפועל סביל"


hitpaelToString : Splits -> Tense -> Person -> Sex -> Quantity -> String
hitpaelToString { p, al } tense person sex quantity =
    let
        ( t, p_ ) =
            hitpaelHelper p

        tpael =
            t ++ p_ ++ al

        ( beginning, end ) =
            beginningAndEnd tense person sex quantity
    in
    case tense of
        Past ->
            "ה" ++ tpael ++ end

        Present ->
            "מ" ++ tpael ++ end

        Future ->
            beginning ++ tpael ++ end

        Imperative ->
            "ה" ++ tpael ++ end


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
