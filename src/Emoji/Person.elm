module Emoji.Person exposing (Person, default, parts, setPart, toString)

import Emoji.Base as Emoji
    exposing
        ( Gender(..)
        , Part(..)
        , Profession
        , SkinTone
        )


type alias Person =
    { gender : Gender
    , skinTone : Maybe SkinTone
    , profession : Maybe Profession
    }


default : Person
default =
    { gender = Woman
    , skinTone = Nothing
    , profession = Nothing
    }


parts : Person -> List Emoji.Part
parts person =
    case ( person.skinTone, person.profession ) of
        ( Nothing, Nothing ) ->
            [ Gender person.gender ]

        ( Just tone, Nothing ) ->
            [ Gender person.gender, SkinTone tone ]

        ( Nothing, Just profession ) ->
            [ Gender person.gender, Zwj, Profession profession ]

        ( Just tone, Just profession ) ->
            [ Gender person.gender, SkinTone tone, Zwj, Profession profession ]


toString : Person -> String
toString person =
    parts person
        |> List.map Emoji.toString
        |> String.join ""


setPart : Part -> Person -> Person
setPart part person =
    case part of
        Gender gender ->
            { person | gender = gender }

        SkinTone skinTone ->
            { person | skinTone = Just skinTone }

        Profession profession ->
            { person | profession = Just profession }

        Zwj ->
            person
