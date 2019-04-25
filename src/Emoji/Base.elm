module Emoji.Base exposing
    ( Gender(..)
    , Part(..)
    , Profession(..)
    , SkinTone(..)
    , samePartTypes
    , title
    , toString
    )


type Part
    = Profession Profession
    | Gender Gender
    | SkinTone SkinTone
    | Zwj


type Profession
    = Health
    | Student
    | Teacher
    | Judge
    | Farmer
    | Cook
    | Mechanic
    | FactoryWorker
    | OfficeWorker
    | Scientist
    | Technologist
    | Singer
    | Artist
    | Pilot
    | Astronaut
    | Firefighter


type Gender
    = Man
    | Woman


type SkinTone
    = Light
    | MediumLight
    | Medium
    | MediumDark
    | Dark


toString : Part -> String
toString part =
    case part of
        Profession prof ->
            professionToString prof

        Gender gender ->
            genderToString gender

        SkinTone tone ->
            skinToneToString tone

        Zwj ->
            zwjToString


title : Part -> String
title part =
    if part == Zwj then
        "[zwj]"

    else
        toString part


professionToString : Profession -> String
professionToString prof =
    case prof of
        Health ->
            "⚕️"

        Student ->
            "🎓"

        Teacher ->
            "🏫"

        Judge ->
            "⚖"

        Farmer ->
            "🌾"

        Cook ->
            "🍳"

        Mechanic ->
            "🔧"

        FactoryWorker ->
            "🏭"

        OfficeWorker ->
            "💼"

        Scientist ->
            "🔬"

        Technologist ->
            "💻"

        Singer ->
            "🎤"

        Artist ->
            "🎨"

        Pilot ->
            "✈"

        Astronaut ->
            "🚀"

        Firefighter ->
            "🚒"


skinToneToString : SkinTone -> String
skinToneToString tone =
    case tone of
        Light ->
            "\x1F3FB"

        MediumLight ->
            "\x1F3FC"

        Medium ->
            "\x1F3FD"

        MediumDark ->
            "\x1F3FE"

        Dark ->
            "\x1F3FF"


genderToString : Gender -> String
genderToString gender =
    case gender of
        Man ->
            "👨"

        Woman ->
            "👩"


zwjToString : String
zwjToString =
    "\x200D"


samePartTypes : Part -> Part -> Bool
samePartTypes p1 p2 =
    case ( p1, p2 ) of
        ( Profession _, Profession _ ) ->
            True

        ( Gender _, Gender _ ) ->
            True

        ( SkinTone _, SkinTone _ ) ->
            True

        ( Zwj, Zwj ) ->
            True

        _ ->
            False
