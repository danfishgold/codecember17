module Hebrew.VerbTest exposing (..)

import Hebrew.Verb as Verb exposing (Verb)
import Hebrew.Base as Base exposing (Tense(..), Person(..), Sex(..), Quantity(..))


sentence : Verb -> String
sentence verb =
    let
        word =
            Verb.toString verb

        prefix =
            case ( verb.person, verb.sex, verb.quantity ) of
                ( First, Male, Singular ) ->
                    "אני (זכר)"

                ( First, Female, Singular ) ->
                    "אני (נקבה)"

                ( First, Male, Plural ) ->
                    "אנחנו (זכר)"

                ( First, Female, Plural ) ->
                    "אנחנו (נקבה)"

                ( Second, Male, Singular ) ->
                    "אתה"

                ( Second, Female, Singular ) ->
                    "את"

                ( Second, Male, Plural ) ->
                    "אתם"

                ( Second, Female, Plural ) ->
                    "אתן"

                ( Third, Male, Singular ) ->
                    "הוא"

                ( Third, Female, Singular ) ->
                    "היא"

                ( Third, Male, Plural ) ->
                    "הם"

                ( Third, Female, Plural ) ->
                    "הן"

        postfix =
            case verb.tense of
                Past ->
                    "בעבר"

                Present ->
                    "בהווה"

                Future ->
                    "בעתיד"

                Imperative ->
                    "בציווי"
    in
        [ prefix, word, postfix ] |> String.join " "


debugSentence : Verb -> String
debugSentence verb =
    [ toString verb.conjugation
    , toString verb.tense
    , toString verb.sex
    , toString verb.quantity
    , toString verb.person
    , Verb.toString verb
    ]
        |> String.join " "


allOptions : List String -> Verb.Conjugation -> List Verb
allOptions root conj =
    let
        allTenses =
            [ Past, Present, Future, Imperative ]

        allPersons =
            [ First, Second, Third ]

        allSexes =
            [ Male, Female ]

        allQuantities =
            [ Singular, Plural ]
    in
        Verb.verb root
            |> Verb.setConjugation conj
            |> List.singleton
            |> (\vs -> List.concatMap (\t -> List.map (Base.setTense t) vs) allTenses)
            |> (\vs -> List.concatMap (\s -> List.map (Base.setSex s) vs) allSexes)
            |> (\vs -> List.concatMap (\q -> List.map (Base.setQuantity q) vs) allQuantities)
            |> (\vs -> List.concatMap (\p -> List.map (Base.setPerson p) vs) allPersons)


test : List String -> Verb.Conjugation -> String
test root conj =
    allOptions root conj |> List.map debugSentence |> String.join "\n"
