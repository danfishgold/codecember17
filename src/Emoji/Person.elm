module Emoji.Person exposing (..)

import Emoji.Base as Emoji exposing (..)


type alias Person =
    { gender : Gender
    , skinTone : SkinTone
    }


default : Person
default =
    { gender = Neutral
    , skinTone = NoTone
    }


parts : Person -> List Emoji.Part
parts prof =
    [ Gender prof.gender, SkinTone prof.skinTone ]


toString : Person -> String
toString prof =
    parts prof
        |> List.map Emoji.toString
        |> String.join ""


setPart : Part -> Person -> Maybe Person
setPart part prof =
    case part of
        Gender gender ->
            Just { prof | gender = gender }

        SkinTone skinTone ->
            Just { prof | skinTone = skinTone }

        Profession profession ->
            Nothing

        Zwj ->
            Nothing
