module Emoji.Professional exposing (..)

import Emoji.Base as Emoji exposing (..)


type alias Professional =
    { profession : Maybe Profession
    , gender : Gender
    , skinTone : Maybe SkinTone
    }


default : Professional
default =
    { profession = Nothing
    , gender = Man
    , skinTone = Nothing
    }


parts : Professional -> List Emoji.Part
parts prof =
    case ( prof.skinTone, prof.profession ) of
        ( Nothing, Nothing ) ->
            [ Gender prof.gender ]

        ( Just tone, Nothing ) ->
            [ Gender prof.gender, SkinTone tone ]

        ( Nothing, Just profession ) ->
            [ Gender prof.gender, Zwj, Profession profession ]

        ( Just tone, Just profession ) ->
            [ Gender prof.gender, SkinTone tone, Zwj, Profession profession ]


toString : Professional -> String
toString prof =
    parts prof
        |> List.map Emoji.toString
        |> String.join ""


setPart : Part -> Professional -> Professional
setPart part prof =
    case part of
        Profession profession ->
            { prof | profession = Just profession }

        Gender gender ->
            { prof | gender = gender }

        SkinTone skinTone ->
            { prof | skinTone = Just skinTone }

        Zwj ->
            prof
