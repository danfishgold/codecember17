module BasicLetters exposing (..)

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
    | Letters (List String)
    | Split
    | Delete


letters : List String
letters =
    "a b c d e f g h i j k l m n o p q r s t u v w x y z" |> String.split " "


sources : List (Category Data)
sources =
    [ { name = "Letters"
      , sources = (letters ++ [ " " ]) |> List.map (Letter >> sourceFromKind)
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

        Letters _ ->
            Color.black


text : Kind -> String
text magnet =
    case magnet of
        Split ->
            "[split]"

        Delete ->
            "[delete]"

        Letter " " ->
            "[space]"

        Letter letter ->
            letter

        Letters letters ->
            letters |> String.join ""


joinStrings : Kind -> Kind -> Maybe Kind
joinStrings left right =
    case ( left, right ) of
        ( Letter l1, Letter l2 ) ->
            Just <| Letters [ l1, l2 ]

        ( Letter l1, Letters ls2 ) ->
            Just <| Letters <| l1 :: ls2

        ( Letters ls1, Letter l2 ) ->
            Just <| Letters <| ls1 ++ [ l2 ]

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

        Letters _ ->
            True

        _ ->
            False


is : Kind -> Magnet Data -> Bool
is kind magnet =
    magnet.data.kind == kind


isCompound : Kind -> Bool
isCompound kind =
    case kind of
        Letters _ ->
            True

        _ ->
            False


interaction : Interaction Data
interaction isSource a b =
    case relativePosition a b of
        Nothing ->
            Nothing

        Just pos ->
            if both a b (mapKind isString) then
                if not isSource then
                    case pos of
                        Left ->
                            Just ( join Left, Color.darkGreen )

                        Right ->
                            Just ( join Right, Color.darkGreen )

                        _ ->
                            Nothing
                else
                    Nothing
            else if either a b (is Delete) then
                Just ( delete, Color.lightRed )
            else if permutation a b (is Split) (mapKind isCompound) /= Nothing then
                Just ( split, Color.darkGreen )
            else
                Nothing


delete : Interactor Data
delete isSource a b =
    case permutation a b (is Delete) (always True) of
        Just ( delete, _ ) ->
            Just ( [], [ { name = "Special", sources = [ delete ] } ] )

        Nothing ->
            Nothing


split : Interactor Data
split isSource a b =
    case permutation a b (is Split) (mapKind isCompound) of
        Just ( split, compound ) ->
            case compound.data.kind of
                Letters letters ->
                    Just
                        ( letters
                            |> List.map (Letter >> magnetFromKind)
                            |> TextRect.organizeInRowAround compound.position 5
                        , [ { name = "Special", sources = [ split ] } ]
                        )

                _ ->
                    Nothing

        Nothing ->
            Nothing


join : RelativePosition -> Bool -> Magnet Data -> Magnet Data -> Maybe ( List (Magnet Data), List (Category Data) )
join rPos isSource a b =
    let
        ( left, right ) =
            if rPos == Left then
                ( a, b )
            else
                ( b, a )
    in
        case joinStrings left.data.kind right.data.kind of
            Nothing ->
                Nothing

            Just kind ->
                Just
                    ( [ magnetFromKind kind
                            |> keepEdgeInPlace (RelativePosition.opposite rPos) b
                            |> setBackground Color.black
                      ]
                    , []
                    )


magnetFromKind : Kind -> Magnet Data
magnetFromKind kind =
    { data = dataFromKind kind
    , text = text kind
    , position = ( 0, 0 )
    , padding = TextRect.defaultPadding
    , highlighted = Nothing
    }
