module Hebrew exposing (..)

import Magnet.Interaction exposing (Interaction, Interactor)
import RelativePosition exposing (RelativePosition(..), relativePosition, keepEdgeInPlace)
import Color exposing (Color)
import Magnet.Base exposing (Magnet, setBackground)
import Magnet.Category exposing (Category)
import TextRect
import Util exposing (Direction(..))
import Hebrew.Base as Base exposing (Tense(..), Person(..), Sex(..), Quantity(..))
import Hebrew.Verb as Verb exposing (Conjugation(..))
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
    | Conj Verb.Conjugation
    | Form Noun.Form
    | Tense Base.Tense
    | Person Base.Person
    | Sex Base.Sex
    | Quantity Base.Quantity
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
    , { name = "Effects"
      , sources =
            [ sourceFromKind <| Conj Paal
            , sourceFromKind <| Conj Nifal
            , sourceFromKind <| Tense Past
            , sourceFromKind <| Tense Present
            , sourceFromKind <| Tense Future
            , sourceFromKind <| Tense Imperative
            , sourceFromKind <| Person First
            , sourceFromKind <| Person Second
            , sourceFromKind <| Person Third
            , sourceFromKind <| Sex Male
            , sourceFromKind <| Sex Female
            , sourceFromKind <| Quantity Singular
            , sourceFromKind <| Quantity Plural
            , sourceFromKind <| Form Katal
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

        Conj _ ->
            Color.darkBrown

        Form _ ->
            Color.darkBrown

        Tense _ ->
            Color.darkPurple

        Person _ ->
            Color.darkPurple

        Sex _ ->
            Color.darkPurple

        Quantity _ ->
            Color.darkPurple

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

        Conj conj ->
            Verb.conjugationTitle conj

        Form form ->
            Noun.formTitle form

        Tense tense ->
            Base.tenseTitle tense

        Person person ->
            Base.personTitle person

        Sex sex ->
            Base.sexTitle sex

        Quantity quantity ->
            Base.quantityTitle quantity

        Verb verb ->
            Verb.toString verb |> Base.withFinalLetters

        Noun noun ->
            Noun.toString noun |> Base.withFinalLetters


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
    Magnet.Interaction.fromInteractors <|
        List.concat
            [ [ ( delete, Color.lightRed )
              , ( always split, Color.darkGreen )
              , ( join, Color.darkGreen )
              ]
            , verbInteractors ++ nounInteractors |> List.map (\interactor -> ( always interactor, Color.darkGreen ))
            ]


verbInteractors : List (Interactor Data)
verbInteractors =
    [ effectInteractor conjugationFromKind verbFromKind Verb Verb.setConjugation
    , effectInteractor tenseFromKind verbFromKind Verb Verb.setTense
    , effectInteractor personFromKind verbFromKind Verb Verb.setPerson
    , effectInteractor sexFromKind verbFromKind Verb Verb.setSex
    , effectInteractor quantityFromKind verbFromKind Verb Verb.setQuantity
    ]


nounInteractors : List (Interactor Data)
nounInteractors =
    [ effectInteractor formFromKind nounFromKind Noun Noun.setForm
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


effectInteractor : (Kind -> Maybe effect) -> (Kind -> Maybe word) -> (word -> Kind) -> (effect -> word -> word) -> Interactor Data
effectInteractor effectFromKind wordFromKind wordKind setEffect isSource a b =
    if not isSource then
        case extractFromPermutation a b (mapKind effectFromKind) (mapKind wordFromKind) of
            Just ( ( effectMagnet, effect ), ( verbMagnet, verb ) ) ->
                setEffect effect verb
                    |> wordKind
                    |> magnetFromKind
                    |> (keepEdgeInPlace RelativePosition.On verbMagnet)
                    |> (\m -> ( [ m ], [ { name = "Effects", sources = [ effectMagnet ] } ] ))
                    |> Just

            Nothing ->
                Nothing
    else
        Nothing


tenseFromKind : Kind -> Maybe Base.Tense
tenseFromKind kind =
    case kind of
        Tense tense ->
            Just tense

        _ ->
            Nothing


personFromKind : Kind -> Maybe Base.Person
personFromKind kind =
    case kind of
        Person person ->
            Just person

        _ ->
            Nothing


sexFromKind : Kind -> Maybe Base.Sex
sexFromKind kind =
    case kind of
        Sex sex ->
            Just sex

        _ ->
            Nothing


quantityFromKind : Kind -> Maybe Base.Quantity
quantityFromKind kind =
    case kind of
        Quantity quantity ->
            Just quantity

        _ ->
            Nothing


formFromKind : Kind -> Maybe Noun.Form
formFromKind kind =
    case kind of
        Form form ->
            Just form

        _ ->
            Nothing


conjugationFromKind : Kind -> Maybe Verb.Conjugation
conjugationFromKind kind =
    case kind of
        Conj conj ->
            Just conj

        _ ->
            Nothing


verbFromKind : Kind -> Maybe Verb.Verb
verbFromKind kind =
    case kind of
        Verb verb ->
            Just verb

        Root root ->
            Just <| Verb.verb root

        _ ->
            Nothing


nounFromKind : Kind -> Maybe Noun.Noun
nounFromKind kind =
    case kind of
        Noun noun ->
            Just noun

        Root root ->
            Just <| Noun.noun root

        _ ->
            Nothing


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


isRootOrNounOrVerb : Kind -> Bool
isRootOrNounOrVerb kind =
    case kind of
        Root _ ->
            True

        Noun _ ->
            True

        Verb _ ->
            True

        _ ->
            False


magnetFromKind : Kind -> Magnet Data
magnetFromKind kind =
    { data = dataFromKind kind
    , text = text kind
    , position = ( 0, 0 )
    , padding = TextRect.defaultPadding
    , highlighted = Nothing
    }
