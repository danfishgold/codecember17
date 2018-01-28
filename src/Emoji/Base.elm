module Emoji.Base exposing (..)


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


type Gender
    = Neutral
    | Man
    | Woman


type SkinTone
    = NoTone
    | Light
    | MediumLight
    | Medium
    | MediumDark
    | Dark


toString : Part -> String
toString part =
    case part of
        Profession prof ->
            professionToString prof ++ variationSelector16

        Gender gender ->
            genderToString gender ++ variationSelector16

        SkinTone tone ->
            skinToneToString tone

        Zwj ->
            zwjToString


title : Part -> String
title part =
    if part == Zwj then
        "[zwj]"
    else
        toString part ++ variationSelector16


professionToString : Profession -> String
professionToString prof =
    case prof of
        Health ->
            "⚕"

        Student ->
            "🎓"

        Teacher ->
            "🏫"

        Judge ->
            "⚖"


skinToneToString : SkinTone -> String
skinToneToString tone =
    case tone of
        NoTone ->
            ""

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
        Neutral ->
            ""

        Man ->
            -- "👨"
            "♂️"

        Woman ->
            -- "👩"
            "♀️"


zwjToString : String
zwjToString =
    "\x200D"


variationSelector16 : String
variationSelector16 =
    "️"
