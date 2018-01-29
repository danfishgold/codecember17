module Emoji.Professional exposing (..)

import Emoji.Base as Emoji exposing (..)


type alias Professional =
    { profession : Profession
    , gender : Gender
    , skinTone : SkinTone
    }


default : Professional
default =
    { profession = Health
    , gender = Neutral
    , skinTone = NoTone
    }


parts : Professional -> List Emoji.Part
parts prof =
    [ Gender prof.gender, SkinTone prof.skinTone, Zwj, Profession prof.profession ]


toString : Professional -> String
toString prof =
    parts prof
        |> List.map Emoji.toString
        |> String.join ""


setPart : Part -> Professional -> Maybe Professional
setPart part prof =
    case part of
        Profession profession ->
            Just { prof | profession = profession }

        Gender gender ->
            Just { prof | gender = gender }

        SkinTone skinTone ->
            Just { prof | skinTone = skinTone }

        Zwj ->
            Nothing
