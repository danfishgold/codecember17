module Emoji exposing (..)

import Magnet.Interaction exposing (Interaction, Interactor)
import RelativePosition exposing (RelativePosition(..), keepEdgeInPlace)
import Color exposing (Color)
import Magnet.Base exposing (Magnet, setBackground)
import Magnet.Category exposing (Category)
import TextRect
import Util exposing (Direction(..), maybeOr)
import Emoji.Base as Emoji exposing (Part(..), Gender(..), SkinTone(..), Profession(..))
import Emoji.Professional as Professional exposing (Professional)
import Emoji.Person as Person exposing (Person)


type alias Data =
    { background : Color
    , textColor : Color
    , kind : Kind
    }


type Kind
    = Atom Emoji.Part
    | Professional Professional
    | Person Person
    | Split
    | Delete


sources : List (Category Data)
sources =
    [ { name = "Characters"
      , sources =
            [ Atom (Gender Man)
            , Atom (Gender Woman)
            , Atom (SkinTone NoTone)
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
    , background = defaultBackground kind
    , textColor = textColor kind
    }


sourceFromKind : Kind -> Magnet Data
sourceFromKind kind =
    Magnet.Base.magnet (text kind) (dataFromKind kind)


textColor : Kind -> Color
textColor kind =
    case kind of
        Split ->
            Color.white

        Delete ->
            Color.white

        _ ->
            Color.black


defaultBackground : Kind -> Color
defaultBackground magnet =
    case magnet of
        Split ->
            Color.darkBlue

        Delete ->
            Color.darkRed

        Atom Zwj ->
            Color.gray

        Atom _ ->
            Color.white

        Professional _ ->
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

        Professional prof ->
            Professional.toString prof

        Person prof ->
            Person.toString prof


joinParts : Part -> Part -> Maybe Kind
joinParts p1 p2 =
    if Emoji.samePartTypes p1 p2 then
        Nothing
    else
        Nothing
            |> maybeOr
                (\_ ->
                    Person.default
                        |> Person.setPart p1
                        |> Maybe.andThen (Person.setPart p2)
                        |> Maybe.map Person
                )
            |> maybeOr
                (\_ ->
                    Professional.default
                        |> Professional.setPart p1
                        |> Maybe.andThen (Professional.setPart p2)
                        |> Maybe.map Professional
                )


either : a -> a -> (a -> Bool) -> Bool
either a b fn =
    fn a || fn b


both : a -> a -> (a -> Bool) -> Bool
both a b fn =
    fn a && fn b


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
        Professional _ ->
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
                Professional prof ->
                    Just
                        ( Professional.parts prof
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
        case ( a.data.kind, b.data.kind ) of
            ( Atom p1, Atom p2 ) ->
                case joinParts p1 p2 of
                    Nothing ->
                        Nothing

                    Just kind ->
                        Just
                            ( [ magnetFromKind kind
                                    |> keepEdgeInPlace (RelativePosition.opposite rPos) b
                                    |> setBackground (defaultBackground kind)
                              ]
                            , []
                            )

            _ ->
                Nothing
    else
        Nothing


magnetFromKind : Kind -> Magnet Data
magnetFromKind kind =
    { data = dataFromKind kind
    , text = text kind
    , position = ( 0, 0 )
    , padding = TextRect.defaultPadding
    , highlighted = Nothing
    }
