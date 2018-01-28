module Emoji exposing (..)

import Magnet.Interaction exposing (Interaction, Interactor)
import RelativePosition exposing (RelativePosition(..), keepEdgeInPlace)
import Color exposing (Color)
import Magnet.Base exposing (Magnet, setBackground)
import Magnet.Category exposing (Category)
import TextRect
import Util exposing (Direction(..))
import Emoji.Base as Emoji exposing (Part(..))
import Emoji.Professional as Professional exposing (Professional)


type alias Data =
    { background : Color
    , textColor : Color
    , kind : Kind
    }


type Kind
    = Atom Emoji.Part
    | Professional Professional
    | Split
    | Delete


sources : List (Category Data)
sources =
    [ { name = "Characters"
      , sources =
            [ Atom Zwj
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
    , textColor = Color.white
    }


sourceFromKind : Kind -> Magnet Data
sourceFromKind kind =
    Magnet.Base.magnet (text kind) (dataFromKind kind)


defaultBackground : Kind -> Color
defaultBackground magnet =
    case magnet of
        Split ->
            Color.darkBlue

        Delete ->
            Color.darkRed

        Atom Zwj ->
            Color.black

        Atom _ ->
            Color.white

        Professional _ ->
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


joinStrings : Kind -> Kind -> Maybe Kind
joinStrings left right =
    case ( left, right ) of
        ( Atom e1, Atom e2 ) ->
            Professional.default
                |> Professional.setPart e1
                |> Professional.setPart e2
                |> Professional
                |> Just

        _ ->
            Nothing


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


mapKind : (Kind -> a) -> Magnet Data -> a
mapKind fn magnet =
    fn magnet.data.kind


isString : Kind -> Bool
isString kind =
    case kind of
        Atom _ ->
            True

        Professional _ ->
            True

        _ ->
            False


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


leftRight : RelativePosition -> Magnet Data -> Magnet Data -> Maybe ( Magnet Data, Magnet Data )
leftRight pos a b =
    case pos of
        Left ->
            Just ( a, b )

        Right ->
            Just ( b, a )

        _ ->
            Nothing


join : RelativePosition -> Bool -> Magnet Data -> Magnet Data -> Maybe ( List (Magnet Data), List (Category Data) )
join rPos isSource a b =
    if not isSource then
        case leftRight rPos a b of
            Just ( left, right ) ->
                case joinStrings left.data.kind right.data.kind of
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

            Nothing ->
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
