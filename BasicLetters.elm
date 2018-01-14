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
    | Word (List String)
    | Sentence (List (List String))
    | UpperCase
    | LowerCase
    | Split
    | Delete


letters : List String
letters =
    "a b c d e f g h i j k l m n o p q r s t u v w x y z" |> String.split " "


sources : List (Category Data)
sources =
    [ { name = "Letters"
      , sources = (letters) |> List.map (Letter >> sourceFromKind)
      }
    , { name = "Special"
      , sources =
            [ sourceFromKind Delete
            , sourceFromKind Split
            , sourceFromKind UpperCase
            , sourceFromKind LowerCase
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

        Word _ ->
            Color.black

        Sentence _ ->
            Color.darkGray

        UpperCase ->
            Color.darkBlue

        LowerCase ->
            Color.darkBlue


text : Kind -> String
text magnet =
    case magnet of
        Split ->
            "[split]"

        Delete ->
            "[delete]"

        UpperCase ->
            "[UPPER]"

        LowerCase ->
            "[lower]"

        Letter letter ->
            letter

        Word letters ->
            letters |> String.join ""

        Sentence words ->
            words |> List.map (String.join "") |> String.join " "


joinStrings : Kind -> Kind -> Maybe Kind
joinStrings left right =
    case ( left, right ) of
        ( Letter l1, Letter l2 ) ->
            Just <| Word [ l1, l2 ]

        ( Word w1, Word w2 ) ->
            Just <| Sentence [ w1, w2 ]

        ( Sentence s1, Sentence s2 ) ->
            Just <| Sentence <| s1 ++ s2

        ( Letter l, Word w ) ->
            Just <| Word <| l :: w

        ( Word w, Letter l ) ->
            Just <| Word <| w ++ [ l ]

        ( Letter l, Sentence s ) ->
            case s of
                [] ->
                    Nothing

                firstWord :: rest ->
                    Just <| Sentence <| (l :: firstWord) :: rest

        ( Sentence s, Letter l ) ->
            case List.head <| List.drop (List.length s - 1) s of
                Nothing ->
                    Nothing

                Just last ->
                    Just <| Sentence <| List.take (List.length s - 1) s ++ [ last ++ [ l ] ]

        ( Word w, Sentence s ) ->
            Just <| Sentence <| w :: s

        ( Sentence s, Word w ) ->
            Just <| Sentence <| s ++ [ w ]

        _ ->
            Nothing


transformString : Kind -> Kind -> Maybe Kind
transformString transformation string =
    case ( transformTransform transformation, string ) of
        ( Just fn, Letter l ) ->
            Just <| Letter <| fn l

        ( Just fn, Word ls ) ->
            Just <| Word <| List.map fn ls

        _ ->
            Nothing


transformTransform : Kind -> Maybe (String -> String)
transformTransform kind =
    case kind of
        UpperCase ->
            Just String.toUpper

        LowerCase ->
            Just String.toLower

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

        Word _ ->
            True

        _ ->
            False


is : Kind -> Magnet Data -> Bool
is kind magnet =
    magnet.data.kind == kind


isCompound : Kind -> Bool
isCompound kind =
    case kind of
        Word _ ->
            True

        Sentence _ ->
            True

        _ ->
            False


isTransformation : Kind -> Bool
isTransformation kind =
    case kind of
        UpperCase ->
            True

        LowerCase ->
            True

        _ ->
            False


interaction : Interaction Data
interaction =
    Magnet.Interaction.fromInteractors
        [ ( delete, Color.lightRed )
        , ( always split, Color.darkGreen )
        , ( always transform, Color.darkGreen )
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
                Word letters ->
                    Just
                        ( letters
                            |> List.map (Letter >> magnetFromKind)
                            |> TextRect.organizeInRowAround compound.position 5
                        , [ { name = "Special", sources = [ split ] } ]
                        )

                Sentence words ->
                    Just
                        ( words
                            |> List.map (Word >> magnetFromKind)
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


transform : Interactor Data
transform isSource a b =
    if not isSource then
        case permutation a b (mapKind isTransformation) (mapKind isString) of
            Just ( transformation, string ) ->
                transformString transformation.data.kind string.data.kind
                    |> Maybe.map magnetFromKind
                    |> Maybe.map (keepEdgeInPlace RelativePosition.On string)
                    |> Maybe.map (\m -> ( [ m ], [ { name = "Special", sources = [ transformation ] } ] ))

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
