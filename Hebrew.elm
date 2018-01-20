module Hebrew exposing (..)

import Magnet.Interaction exposing (Interaction, Interactor)
import RelativePosition exposing (RelativePosition(..), relativePosition, keepEdgeInPlace)
import Color exposing (Color)
import Magnet.Base exposing (Magnet, setBackground)
import Magnet.Category exposing (Category)
import TextRect
import Util exposing (Direction(..))
import Hebrew.Base as Base exposing (Tense(..), Person(..), Sex(..), Quantity(..))
import Hebrew.Verb as Verb exposing (Conjugation(..), Effect)
import Hebrew.Noun as Noun exposing (Form(..))


type alias Data =
    { background : Color
    , textColor : Color
    , kind : Kind
    }


type Kind
    = Letter String
    | Root (List String)
    | Verb Verb.Verb
    | Noun Noun.Noun
    | Effect Verb.Effect
    | Form Noun.Form
    | Split
    | Delete


letters : List String
letters =
    "א ב ג ד ה ו ז ח ט י כ ל מ נ ס ע פ צ ק ר ש ת"
        |> String.split " "


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
    , { name = "Verb Effects"
      , sources =
            [ sourceFromKind <| Effect <| Verb.Conj Paal
            , sourceFromKind <| Effect <| Verb.Conj Nifal
            , sourceFromKind <| Effect <| Verb.Tense Past
            , sourceFromKind <| Effect <| Verb.Tense Present
            , sourceFromKind <| Effect <| Verb.Tense Future
            , sourceFromKind <| Effect <| Verb.Tense Imperative
            , sourceFromKind <| Effect <| Verb.Person First
            , sourceFromKind <| Effect <| Verb.Person Second
            , sourceFromKind <| Effect <| Verb.Person Third
            , sourceFromKind <| Effect <| Verb.Sex Male
            , sourceFromKind <| Effect <| Verb.Sex Female
            , sourceFromKind <| Effect <| Verb.Quantity Singular
            , sourceFromKind <| Effect <| Verb.Quantity Plural
            ]
      }
    , { name = "Noun Effects"
      , sources =
            [ sourceFromKind <| Form Katal
            , sourceFromKind <| Form Miktal
            , sourceFromKind <| Form Miktala
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

        Effect _ ->
            Color.darkPurple

        Form _ ->
            Color.darkBrown

        Verb _ ->
            Color.darkGray

        Noun _ ->
            Color.darkGray


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

        Effect effect ->
            Verb.effectTitle effect

        Form form ->
            Noun.formTitle form

        Verb verb ->
            Verb.toString verb

        Noun noun ->
            Noun.toString noun


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
        , ( always verbEffect, Color.darkGreen )
        , ( always nounEffect, Color.darkGreen )
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
                            |> TextRect.organizeInRowAround Rtl compound.position 5
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


isEffect : Kind -> Bool
isEffect kind =
    case kind of
        Effect _ ->
            True

        _ ->
            False


isForm : Kind -> Bool
isForm kind =
    case kind of
        Form _ ->
            True

        _ ->
            False


isRootOrVerb : Kind -> Bool
isRootOrVerb kind =
    case kind of
        Root _ ->
            True

        Verb _ ->
            True

        _ ->
            False


isRootOrNoun : Kind -> Bool
isRootOrNoun kind =
    case kind of
        Root _ ->
            True

        Noun _ ->
            True

        _ ->
            False


verbEffect : Interactor Data
verbEffect isSource a b =
    if not isSource then
        case permutation a b (mapKind isEffect) (mapKind isRootOrVerb) of
            Just ( effect, verbThing ) ->
                let
                    newVerbKind =
                        case ( verbThing.data.kind, effect.data.kind ) of
                            ( Verb verb, Effect effect ) ->
                                Just <| Verb <| Verb.apply effect verb

                            ( Root root, Effect effect ) ->
                                Just <| Verb <| Verb.apply effect <| Verb.verb root

                            _ ->
                                Nothing

                    data =
                        verbThing.data
                in
                    newVerbKind
                        |> Maybe.map magnetFromKind
                        |> Maybe.map (keepEdgeInPlace RelativePosition.On verbThing)
                        |> Maybe.map (\m -> ( [ m ], [ { name = "Verb Effects", sources = [ effect ] } ] ))

            Nothing ->
                Nothing
    else
        Nothing


nounEffect : Interactor Data
nounEffect isSource a b =
    if not isSource then
        case permutation a b (mapKind isForm) (mapKind isRootOrNoun) of
            Just ( effect, nounThing ) ->
                let
                    newNounKind =
                        case ( nounThing.data.kind, effect.data.kind ) of
                            ( Noun noun, Form form ) ->
                                Just <| Noun <| Noun.setForm form noun

                            ( Root root, Form form ) ->
                                Just <| Noun <| Noun.setForm form <| Noun.noun root

                            _ ->
                                Nothing

                    data =
                        nounThing.data
                in
                    newNounKind
                        |> Maybe.map magnetFromKind
                        |> Maybe.map (keepEdgeInPlace RelativePosition.On nounThing)
                        |> Maybe.map (\m -> ( [ m ], [ { name = "Noun Effects", sources = [ effect ] } ] ))

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
