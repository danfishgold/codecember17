module Hebrew exposing (..)

import Magnet.Interaction exposing (Interaction, Interactor)
import RelativePosition exposing (RelativePosition(..), relativePosition, keepEdgeInPlace)
import Color exposing (Color)
import Magnet.Base exposing (Magnet, setBackground)
import Magnet.Category exposing (Category)
import TextRect


type alias Data =
    { background : Color
    , textColor : Color
    , kind : Kind
    }


type Kind
    = Letter String
    | Root (List String)
    | Split
    | Delete


letters : List String
letters =
    "א ב ג ד ה ו ז ח ט י כ ל מ נ ס ע פ צ ק ר ש ת"
        |> String.split " "
        |> List.reverse


sources : List (Category Data)
sources =
    [ { name = "Letters"
      , sources = (letters) |> List.map (Letter >> sourceFromKind)
      }
    , { name = "Special"
      , sources =
            [ sourceFromKind Delete
            , sourceFromKind Split
            ]
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

        Letter _ ->
            Color.black

        Root _ ->
            Color.black


text : Kind -> String
text magnet =
    case magnet of
        Split ->
            "[פיצול]"

        Delete ->
            "[מחיקה]"

        Letter letter ->
            letter

        Root letters ->
            letters |> String.join "."


joinStrings : Kind -> Kind -> Maybe Kind
joinStrings left right =
    case ( right, left ) of
        ( Letter l1, Letter l2 ) ->
            Just <| Root [ l1, l2 ]

        ( Root w1, Root w2 ) ->
            Just <| Root <| w1 ++ w2

        ( Letter l, Root r ) ->
            Just <| Root <| l :: r

        ( Root r, Letter l ) ->
            Just <| Root <| r ++ [ l ]

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


mapKind : (Kind -> a) -> Magnet Data -> a
mapKind fn magnet =
    fn magnet.data.kind


isString : Kind -> Bool
isString kind =
    case kind of
        Letter _ ->
            True

        Root _ ->
            True

        _ ->
            False


is : Kind -> Magnet Data -> Bool
is kind magnet =
    magnet.data.kind == kind


isCompound : Kind -> Bool
isCompound kind =
    case kind of
        Root _ ->
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
                Root letters ->
                    Just
                        ( letters
                            |> List.map (Letter >> magnetFromKind)
                            |> List.reverse
                            |> TextRect.organizeInRowAround compound.position 5
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
