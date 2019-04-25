module Hebrew exposing (Data, environment)

import Color exposing (Color)
import Hebrew.Base as Base exposing (Person(..), Quantity(..), Sex(..), Tense(..))
import Hebrew.Noun as Noun exposing (ConstructState(..), Form(..))
import Hebrew.Verb as Verb exposing (Conjugation(..))
import Magnet
import Magnet.Base exposing (Magnet, setBackground)
import Magnet.Category exposing (Category)
import Magnet.Interaction exposing (Interaction, Interactor)
import RelativePosition exposing (RelativePosition(..), keepEdgeInPlace)
import TextRect
import Util exposing (Direction(..))


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
    | Construct Noun.Construct
    | Conj Verb.Conjugation
    | Form Noun.Form
    | Tense Base.Tense
    | Person Base.Person
    | ChangeSex
    | ChangeQuantity
    | ChangeDefiniteness
    | ChangeConstructState
    | Split
    | Delete


environment : Magnet.Environment Data
environment =
    { sources = sources
    , interaction = interaction
    , sourcesDirection = Rtl
    }


letters : List String
letters =
    "א ב ג ד ה ו ז ח ט י כ ל מ נ ס ע פ צ ק ר ש ת"
        |> String.split " "


sources : List (Category Data)
sources =
    [ { name = "Letters"
      , sources = letters |> List.map (Letter >> sourceFromKind)
      }
    , { name = "Special"
      , sources =
            [ Delete
            , Split
            ]
                |> List.map sourceFromKind
      }
    , { name = "Effects"
      , sources =
            [ Conj Paal
            , Conj Nifal
            , Conj Hifil
            , Conj Hufal
            , Conj Piel
            , Conj Pual
            , Conj Hitpael
            , Form Katal
            , Form Miktal
            , Form Katelet
            , Form Miktala
            , Form Katlia
            , Tense Past
            , Tense Present
            , Tense Future
            , Tense Imperative
            , Person First
            , Person Second
            , Person Third
            , ChangeSex
            , ChangeQuantity
            , ChangeDefiniteness
            , ChangeConstructState
            ]
                |> List.map sourceFromKind
      }
    , { name = "Roots"
      , sources =
            [ [ "ס", "פ", "ר" ]
            , [ "מ", "ש", "ל" ]
            ]
                |> List.map (Root >> sourceFromKind)
      }
    ]


dataFromKind : Kind -> Magnet.Base.Data Data
dataFromKind kind =
    { kind = kind
    , background = background kind
    , textColor = Color.white
    }


sourceFromKind : Kind -> Magnet Data
sourceFromKind kind =
    Magnet.Base.magnet (text kind) 18 (dataFromKind kind)


background : Kind -> Color
background magnet =
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
            Color.darkYellow

        Tense _ ->
            Color.rgb 64 48 117

        Person _ ->
            Color.rgb 111 37 111

        ChangeSex ->
            Color.rgb 152 51 82

        ChangeQuantity ->
            Color.rgb 152 51 82

        ChangeConstructState ->
            Color.rgb 152 51 82

        ChangeDefiniteness ->
            Color.rgb 152 51 82

        Verb _ ->
            Color.darkGray

        Noun _ ->
            Color.darkGray

        Construct _ ->
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

        ChangeSex ->
            "זכר/נקבה"

        ChangeQuantity ->
            "יחיד/רבים"

        ChangeDefiniteness ->
            "עם/בלי יידוע"

        ChangeConstructState ->
            "סומך/נסמך"

        Verb verb ->
            Verb.toString verb |> Base.withFinalLetters

        Noun noun ->
            Noun.toString noun |> Base.withFinalLetters

        Construct construct ->
            Noun.constructToString construct |> Base.withFinalLetters


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

        ( Noun n1, Noun n2 ) ->
            Just <| Construct <| Noun.construct [ n1, n2 ]

        ( Noun n, Construct c ) ->
            Just <| Construct <| Noun.construct (n :: c.nouns)

        ( Construct c, Noun n ) ->
            Just <| Construct <| Noun.construct <| c.nouns ++ [ n ]

        ( Construct c1, Construct c2 ) ->
            Just <| Construct <| Noun.construct <| c1.nouns ++ c2.nouns

        _ ->
            Nothing


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


is : Kind -> Magnet Data -> Bool
is kind magnet =
    magnet.data.kind == kind


isCompound : Kind -> Bool
isCompound kind =
    case kind of
        Root _ ->
            True

        Construct _ ->
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
            , List.concat
                [ verbInteractors
                , nounInteractors
                , wordInteractors
                , constructInteractors
                ]
                |> List.map (\interactor -> ( always interactor, Color.darkGreen ))
            ]


verbInteractors : List (Interactor Data)
verbInteractors =
    [ effectInteractor tenseFromKind verbFromVerbLike Verb Base.setTense
    , effectInteractor personFromKind verbFromVerbLike Verb Base.setPerson
    , effectInteractor sexFromKind verbFromVerbLike Verb (always Base.changeSex)
    , effectInteractor quantityFromKind verbFromVerbLike Verb (always Base.changeQuantity)
    ]


nounInteractors : List (Interactor Data)
nounInteractors =
    [ effectInteractor quantityFromKind nounFromNounLike Noun (always Base.changeQuantity)
    , effectInteractor constructStateFromKind nounFromNounLike Noun (always Noun.changeConstructState)
    , effectInteractor definitenessFromKind nounFromNounLike Noun (always Noun.changeDefinite)
    ]


constructInteractors : List (Interactor Data)
constructInteractors =
    [ effectInteractor definitenessFromKind constructFromKind Construct (always Noun.changeDefinite)
    ]


wordInteractors : List (Interactor Data)
wordInteractors =
    [ effectInteractor formFromKind nounFromWordLike Noun Noun.setForm
    , effectInteractor conjugationFromKind verbFromWordLike Verb Verb.setConjugation
    ]


delete : RelativePosition -> Interactor Data
delete pos _ a b =
    if pos == On then
        case permutation a b (is Delete) (always True) of
            Just ( del, _ ) ->
                Just ( [], [ { name = "Special", sources = [ del ] } ] )

            Nothing ->
                Nothing

    else
        Nothing


split : Interactor Data
split _ a b =
    case permutation a b (is Split) (mapKind isCompound) of
        Just ( spl, compound ) ->
            case compound.data.kind of
                Root letters ->
                    Just
                        ( letters
                            |> List.map (Letter >> magnetFromKind)
                            |> TextRect.organizeInRowAround Rtl compound.position 5
                        , [ { name = "Special", sources = [ spl ] } ]
                        )

                Construct construct ->
                    Just
                        ( Noun.split construct
                            |> List.map (Noun >> magnetFromKind)
                            |> TextRect.organizeInRowAround Rtl compound.position 5
                        , [ { name = "Special", sources = [ spl ] } ]
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
                                    |> setBackground (background kind)
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
                    |> keepEdgeInPlace RelativePosition.On verbMagnet
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


sexFromKind : Kind -> Maybe ()
sexFromKind kind =
    case kind of
        ChangeSex ->
            Just ()

        _ ->
            Nothing


quantityFromKind : Kind -> Maybe ()
quantityFromKind kind =
    case kind of
        ChangeQuantity ->
            Just ()

        _ ->
            Nothing


constructStateFromKind : Kind -> Maybe ()
constructStateFromKind kind =
    case kind of
        ChangeConstructState ->
            Just ()

        _ ->
            Nothing


definitenessFromKind : Kind -> Maybe ()
definitenessFromKind kind =
    case kind of
        ChangeDefiniteness ->
            Just ()

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


verbFromVerbLike : Kind -> Maybe Verb.Verb
verbFromVerbLike kind =
    case kind of
        Verb verb ->
            Just verb

        Root root ->
            Just <| Verb.verb root

        _ ->
            Nothing


verbFromWordLike : Kind -> Maybe Verb.Verb
verbFromWordLike kind =
    case kind of
        Verb verb ->
            Just verb

        Root root ->
            Just <| Verb.verb root

        Noun noun ->
            Just <| Base.setQuantity noun.quantity <| Verb.verb noun.root

        _ ->
            Nothing


nounFromNounLike : Kind -> Maybe Noun.Noun
nounFromNounLike kind =
    case kind of
        Noun noun ->
            Just noun

        Root root ->
            Just <| Noun.noun root

        _ ->
            Nothing


nounFromWordLike : Kind -> Maybe Noun.Noun
nounFromWordLike kind =
    case kind of
        Noun noun ->
            Just noun

        Root root ->
            Just <| Noun.noun root

        Verb verb ->
            Just <| Base.setQuantity verb.quantity <| Noun.noun verb.root

        _ ->
            Nothing


constructFromKind : Kind -> Maybe Noun.Construct
constructFromKind kind =
    case kind of
        Construct construct ->
            Just construct

        _ ->
            Nothing


magnetFromKind : Kind -> Magnet Data
magnetFromKind kind =
    { data = dataFromKind kind
    , text = text kind
    , textSize = 18
    , position = ( 0, 0 )
    , padding = TextRect.defaultPadding
    , highlighted = Nothing
    }
