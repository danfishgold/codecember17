module Emoji exposing (..)

import Magnet.Interaction exposing (Interaction, Interactor)
import RelativePosition exposing (RelativePosition(..), keepEdgeInPlace)
import Color exposing (Color)
import Magnet.Base exposing (Magnet, setBackground)
import Magnet.Category exposing (Category)
import Magnet
import TextRect
import Util exposing (Direction(..), maybeOr)
import Emoji.Base as Emoji exposing (Part(..), Gender(..), SkinTone(..), Profession(..))
import Emoji.Person as Person exposing (Person)


type alias Data =
    { background : Color
    , textColor : Color
    , kind : Kind
    }


type Kind
    = Atom Emoji.Part
    | Person Person
    | Split
    | Delete


environment : Magnet.Environment Data
environment =
    { sources = sources
    , interaction = interaction
    , sourcesDirection = Ltr
    }


sources : List (Category Data)
sources =
    [ { name = "Characters"
      , sources =
            [ Atom (Gender Man)
            , Atom (Gender Woman)
            , Atom (SkinTone Light)
            , Atom (SkinTone MediumLight)
            , Atom (SkinTone Medium)
            , Atom (SkinTone MediumDark)
            , Atom (SkinTone Dark)
            , Atom (Profession Health)
            , Atom (Profession Student)
            , Atom (Profession Teacher)
            , Atom (Profession Judge)
            , Atom (Profession Farmer)
            , Atom (Profession Cook)
            , Atom (Profession Mechanic)
            , Atom (Profession FactoryWorker)
            , Atom (Profession OfficeWorker)
            , Atom (Profession Scientist)
            , Atom (Profession Technologist)
            , Atom (Profession Singer)
            , Atom (Profession Artist)
            , Atom (Profession Pilot)
            , Atom (Profession Astronaut)
            , Atom (Profession Firefighter)
            ]
                |> List.map sourceFromKind
      }
    , { name = "Special"
      , sources =
            [ Delete
            , Split
            ]
                |> List.map sourceFromKind
      }
    ]


dataFromKind : Kind -> Magnet.Base.Data Data
dataFromKind kind =
    { kind = kind
    , background = background kind
    , textColor = textColor kind
    }


sourceFromKind : Kind -> Magnet Data
sourceFromKind kind =
    Magnet.Base.magnet (text kind) (textSize kind) (dataFromKind kind)


textColor : Kind -> Color
textColor kind =
    case kind of
        Split ->
            Color.white

        Delete ->
            Color.white

        _ ->
            Color.black


background : Kind -> Color
background magnet =
    case magnet of
        Split ->
            Color.darkBlue

        Delete ->
            Color.darkRed

        Atom Zwj ->
            Color.gray

        Atom _ ->
            Color.white

        Person _ ->
            Color.white


text : Kind -> String
text magnet =
    case magnet of
        Split ->
            "[split]"

        Delete ->
            "[delete]"

        Atom part ->
            Emoji.title part

        Person person ->
            Person.toString person


textSize : Kind -> Int
textSize kind =
    case kind of
        Split ->
            18

        Delete ->
            18

        _ ->
            60


permutation : a -> a -> (a -> Bool) -> (a -> Bool) -> Maybe ( a, a )
permutation a b fn1 fn2 =
    if fn1 a && fn2 b then
        Just ( a, b )
    else if fn1 b && fn2 a then
        Just ( b, a )
    else
        Nothing


mapKind : (Kind -> a) -> Magnet Data -> a
mapKind fn magnet =
    fn magnet.data.kind


is : Kind -> Magnet Data -> Bool
is kind magnet =
    magnet.data.kind == kind


isCompound : Kind -> Bool
isCompound kind =
    case kind of
        Person _ ->
            True

        _ ->
            False


interaction : Interaction Data
interaction =
    Magnet.Interaction.fromInteractors
        [ ( delete, Color.lightRed )
        , ( always split, Color.darkGreen )
        , ( join, Color.darkGreen )
        ]


delete : RelativePosition -> Interactor Data
delete pos isSource a b =
    if pos == On then
        case permutation a b (is Delete) (always True) of
            Just ( delete, _ ) ->
                Just ( [], [ { name = "Special", sources = [ delete ] } ] )

            Nothing ->
                Nothing
    else
        Nothing


split : Interactor Data
split isSource a b =
    case permutation a b (is Split) (mapKind isCompound) of
        Just ( split, compound ) ->
            case compound.data.kind of
                Person person ->
                    Just
                        ( Person.parts person
                            |> List.map (Atom >> magnetFromKind)
                            |> TextRect.organizeInRowAround Ltr compound.position 5
                        , [ { name = "Special", sources = [ split ] } ]
                        )

                _ ->
                    Nothing

        Nothing ->
            Nothing


join : RelativePosition -> Bool -> Magnet Data -> Magnet Data -> Maybe ( List (Magnet Data), List (Category Data) )
join rPos isSource a b =
    if not isSource then
        case joiner a.data.kind b.data.kind of
            Just kind ->
                Just
                    ( [ magnetFromKind kind
                            |> keepEdgeInPlace (RelativePosition.opposite rPos) b
                            |> setBackground (background kind)
                      ]
                    , []
                    )

            _ ->
                Nothing
    else
        Nothing


joiner : Kind -> Kind -> Maybe Kind
joiner a b =
    case extractFromPermutation a b partFromKind partFromKind of
        Just ( ( _, part1 ), ( _, part2 ) ) ->
            if Emoji.samePartTypes part1 part2 then
                Nothing
            else
                Person.default
                    |> Person.setPart part1
                    |> Person.setPart part2
                    |> Person
                    |> Just

        Nothing ->
            case extractFromPermutation a b personFromKind partFromKind of
                Just ( ( _, person ), ( _, part ) ) ->
                    person |> Person.setPart part |> Person |> Just

                Nothing ->
                    Nothing


extractFromPermutation : a -> a -> (a -> Maybe b) -> (a -> Maybe c) -> Maybe ( ( a, b ), ( a, c ) )
extractFromPermutation a b fn1 fn2 =
    case ( fn1 a, fn2 b ) of
        ( Just fa, Just fb ) ->
            Just ( ( a, fa ), ( b, fb ) )

        _ ->
            case ( fn1 b, fn2 a ) of
                ( Just fb, Just fa ) ->
                    Just ( ( b, fb ), ( a, fa ) )

                _ ->
                    Nothing


partFromKind : Kind -> Maybe Part
partFromKind kind =
    case kind of
        Atom part ->
            Just part

        _ ->
            Nothing


personFromKind : Kind -> Maybe Person
personFromKind kind =
    case kind of
        Person person ->
            Just person

        _ ->
            Nothing


magnetFromKind : Kind -> Magnet Data
magnetFromKind kind =
    { data = dataFromKind kind
    , text = text kind
    , textSize = textSize kind
    , position = ( 0, 0 )
    , padding = TextRect.defaultPadding
    , highlighted = Nothing
    }
