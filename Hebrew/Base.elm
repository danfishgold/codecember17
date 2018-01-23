module Hebrew.Base exposing (..)


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


tenseTitle : Tense -> String
tenseTitle tense =
    case tense of
        Past ->
            "עבר"

        Present ->
            "הווה"

        Future ->
            "עתיד"

        Imperative ->
            "ציווי"


personTitle : Person -> String
personTitle person =
    case person of
        First ->
            "גוף ראשון"

        Second ->
            "גוף שני"

        Third ->
            "גוף שלישי"


sexTitle : Sex -> String
sexTitle sex =
    case sex of
        Male ->
            "זכר"

        Female ->
            "נקבה"


quantityTitle : Quantity -> String
quantityTitle quantity =
    case quantity of
        Singular ->
            "יחיד"

        Plural ->
            "רבים"


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


setQuantity : Quantity -> { word | quantity : Quantity } -> { word | quantity : Quantity }
setQuantity quantity noun =
    { noun | quantity = quantity }


setTense : Tense -> { word | tense : Tense } -> { word | tense : Tense }
setTense tense verb =
    { verb | tense = tense }


setPerson : Person -> { word | person : Person } -> { word | person : Person }
setPerson person verb =
    { verb | person = person }


setSex : Sex -> { word | sex : Sex } -> { word | sex : Sex }
setSex sex verb =
    { verb | sex = sex }
