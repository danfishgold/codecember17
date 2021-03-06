module Tests exposing (describeVerb, main, suite, testVerb)

import Expect exposing (Expectation)
import Hebrew.Base as Base exposing (Person(..), Quantity(..), Sex(..), Tense(..))
import Hebrew.Verb as Verb exposing (Conjugation(..), Verb)
import Test exposing (..)
import Test.Runner.Html


describeVerb : Verb -> String
describeVerb verb =
    [ verb.root |> String.join "."
    , Verb.conjugationTitle verb.conjugation
    , Base.tenseTitle verb.tense
    , Base.sexTitle verb.sex
    , Base.quantityTitle verb.quantity
    , Base.personTitle verb.person
    ]
        |> String.join " "


testVerb : String -> Conjugation -> Tense -> Sex -> Quantity -> Person -> String -> Test
testVerb rootString conj tense sex quantity person word =
    let
        verb =
            { root = String.split "" rootString
            , conjugation = conj
            , tense = tense
            , sex = sex
            , quantity = quantity
            , person = person
            }
    in
    test (describeVerb verb) <| \_ -> Expect.equal (Verb.toString verb) word


suite : Test
suite =
    describe "Hebrew Verbs"
        [ describe "קל"
            [ testVerb "פעל" Paal Past Male Singular First "פעלתי"
            , testVerb "פעל" Paal Present Male Singular First "פועל"
            , testVerb "פעל" Paal Future Male Singular First "אפעל"
            , testVerb "פעל" Paal Past Female Singular First "פעלתי"
            , testVerb "פעל" Paal Present Female Singular First "פועלת"
            , testVerb "פעל" Paal Future Female Singular First "אפעל"
            , testVerb "פעל" Paal Past Male Plural First "פעלנו"
            , testVerb "פעל" Paal Present Male Plural First "פועלים"
            , testVerb "פעל" Paal Future Male Plural First "נפעל"
            , testVerb "פעל" Paal Past Female Plural First "פעלנו"
            , testVerb "פעל" Paal Present Female Plural First "פועלות"
            , testVerb "פעל" Paal Future Female Plural First "נפעל"
            , testVerb "פעל" Paal Past Male Singular Second "פעלת"
            , testVerb "פעל" Paal Present Male Singular Second "פועל"
            , testVerb "פעל" Paal Future Male Singular Second "תפעל"
            , testVerb "פעל" Paal Past Female Singular Second "פעלת"
            , testVerb "פעל" Paal Present Female Singular Second "פועלת"
            , testVerb "פעל" Paal Future Female Singular Second "תפעלי"
            , testVerb "פעל" Paal Past Male Plural Second "פעלתם"
            , testVerb "פעל" Paal Present Male Plural Second "פועלים"
            , testVerb "פעל" Paal Future Male Plural Second "תפעלו"
            , testVerb "פעל" Paal Past Female Plural Second "פעלתן"
            , testVerb "פעל" Paal Present Female Plural Second "פועלות"
            , testVerb "פעל" Paal Future Female Plural Second "תפעלנה"
            , testVerb "פעל" Paal Past Male Singular Third "פעל"
            , testVerb "פעל" Paal Present Male Singular Third "פועל"
            , testVerb "פעל" Paal Future Male Singular Third "יפעל"
            , testVerb "פעל" Paal Past Female Singular Third "פעלה"
            , testVerb "פעל" Paal Present Female Singular Third "פועלת"
            , testVerb "פעל" Paal Future Female Singular Third "תפעל"
            , testVerb "פעל" Paal Past Male Plural Third "פעלו"
            , testVerb "פעל" Paal Present Male Plural Third "פועלים"
            , testVerb "פעל" Paal Future Male Plural Third "יפעלו"
            , testVerb "פעל" Paal Past Female Plural Third "פעלו"
            , testVerb "פעל" Paal Present Female Plural Third "פועלות"
            , testVerb "פעל" Paal Future Female Plural Third "תפעלנה"
            , testVerb "פעל" Paal Imperative Male Singular First "פעל"
            , testVerb "פעל" Paal Imperative Female Singular First "פעלי"
            , testVerb "פעל" Paal Imperative Male Plural First "פעלו"
            , testVerb "פעל" Paal Imperative Female Plural First "פעלנה"
            ]
        , describe "נפעל"
            [ testVerb "פעל" Nifal Past Male Singular First "נפעלתי"
            , testVerb "פעל" Nifal Present Male Singular First "נפעל"
            , testVerb "פעל" Nifal Future Male Singular First "אפעל"
            , testVerb "פעל" Nifal Past Female Singular First "נפעלתי"
            , testVerb "פעל" Nifal Present Female Singular First "נפעלת"
            , testVerb "פעל" Nifal Future Female Singular First "אפעל"
            , testVerb "פעל" Nifal Past Male Plural First "נפעלנו"
            , testVerb "פעל" Nifal Present Male Plural First "נפעלים"
            , testVerb "פעל" Nifal Future Male Plural First "נפעל"
            , testVerb "פעל" Nifal Past Female Plural First "נפעלנו"
            , testVerb "פעל" Nifal Present Female Plural First "נפעלות"
            , testVerb "פעל" Nifal Future Female Plural First "נפעל"
            , testVerb "פעל" Nifal Past Male Singular Second "נפעלת"
            , testVerb "פעל" Nifal Present Male Singular Second "נפעל"
            , testVerb "פעל" Nifal Future Male Singular Second "תפעל"
            , testVerb "פעל" Nifal Past Female Singular Second "נפעלת"
            , testVerb "פעל" Nifal Present Female Singular Second "נפעלת"
            , testVerb "פעל" Nifal Future Female Singular Second "תפעלי"
            , testVerb "פעל" Nifal Past Male Plural Second "נפעלתם"
            , testVerb "פעל" Nifal Present Male Plural Second "נפעלים"
            , testVerb "פעל" Nifal Future Male Plural Second "תפעלו"
            , testVerb "פעל" Nifal Past Female Plural Second "נפעלתן"
            , testVerb "פעל" Nifal Present Female Plural Second "נפעלות"
            , testVerb "פעל" Nifal Future Female Plural Second "תפעלנה"
            , testVerb "פעל" Nifal Past Male Singular Third "נפעל"
            , testVerb "פעל" Nifal Present Male Singular Third "נפעל"
            , testVerb "פעל" Nifal Future Male Singular Third "יפעל"
            , testVerb "פעל" Nifal Past Female Singular Third "נפעלה"
            , testVerb "פעל" Nifal Present Female Singular Third "נפעלת"
            , testVerb "פעל" Nifal Future Female Singular Third "תפעל"
            , testVerb "פעל" Nifal Past Male Plural Third "נפעלו"
            , testVerb "פעל" Nifal Present Male Plural Third "נפעלים"
            , testVerb "פעל" Nifal Future Male Plural Third "יפעלו"
            , testVerb "פעל" Nifal Past Female Plural Third "נפעלו"
            , testVerb "פעל" Nifal Present Female Plural Third "נפעלות"
            , testVerb "פעל" Nifal Future Female Plural Third "תפעלנה"
            , testVerb "פעל" Nifal Imperative Male Singular First "הפעל"
            , testVerb "פעל" Nifal Imperative Female Singular First "הפעלי"
            , testVerb "פעל" Nifal Imperative Male Plural First "הפעלו"
            , testVerb "פעל" Nifal Imperative Female Plural First "הפעלנה"
            ]
        , describe "הפעיל"
            [ testVerb "פעל" Hifil Past Male Singular First "הפעלתי"
            , testVerb "פעל" Hifil Present Male Singular First "מפעיל"
            , testVerb "פעל" Hifil Future Male Singular First "אפעיל"
            , testVerb "פעל" Hifil Past Female Singular First "הפעלתי"
            , testVerb "פעל" Hifil Present Female Singular First "מפעילה"
            , testVerb "פעל" Hifil Future Female Singular First "אפעיל"
            , testVerb "פעל" Hifil Past Male Plural First "הפעלנו"
            , testVerb "פעל" Hifil Present Male Plural First "מפעילים"
            , testVerb "פעל" Hifil Future Male Plural First "נפעיל"
            , testVerb "פעל" Hifil Past Female Plural First "הפעלנו"
            , testVerb "פעל" Hifil Present Female Plural First "מפעילות"
            , testVerb "פעל" Hifil Future Female Plural First "נפעיל"
            , testVerb "פעל" Hifil Past Male Singular Second "הפעלת"
            , testVerb "פעל" Hifil Present Male Singular Second "מפעיל"
            , testVerb "פעל" Hifil Future Male Singular Second "תפעיל"
            , testVerb "פעל" Hifil Past Female Singular Second "הפעלת"
            , testVerb "פעל" Hifil Present Female Singular Second "מפעילה"
            , testVerb "פעל" Hifil Future Female Singular Second "תפעילי"
            , testVerb "פעל" Hifil Past Male Plural Second "הפעלתם"
            , testVerb "פעל" Hifil Present Male Plural Second "מפעילים"
            , testVerb "פעל" Hifil Future Male Plural Second "תפעילו"
            , testVerb "פעל" Hifil Past Female Plural Second "הפעלתן"
            , testVerb "פעל" Hifil Present Female Plural Second "מפעילות"
            , testVerb "פעל" Hifil Future Female Plural Second "תפעילנה"
            , testVerb "פעל" Hifil Past Male Singular Third "הפעיל"
            , testVerb "פעל" Hifil Present Male Singular Third "מפעיל"
            , testVerb "פעל" Hifil Future Male Singular Third "יפעיל"
            , testVerb "פעל" Hifil Past Female Singular Third "הפעילה"
            , testVerb "פעל" Hifil Present Female Singular Third "מפעילה"
            , testVerb "פעל" Hifil Future Female Singular Third "תפעיל"
            , testVerb "פעל" Hifil Past Male Plural Third "הפעילו"
            , testVerb "פעל" Hifil Present Male Plural Third "מפעילים"
            , testVerb "פעל" Hifil Future Male Plural Third "יפעילו"
            , testVerb "פעל" Hifil Past Female Plural Third "הפעילו"
            , testVerb "פעל" Hifil Present Female Plural Third "מפעילות"
            , testVerb "פעל" Hifil Future Female Plural Third "תפעילנה"
            , testVerb "פעל" Hifil Imperative Male Singular First "הפעל"
            , testVerb "פעל" Hifil Imperative Female Singular First "הפעילי"
            , testVerb "פעל" Hifil Imperative Male Plural First "הפעילו"
            , testVerb "פעל" Hifil Imperative Female Plural First "הפעלנה"
            ]
        , describe "הופעל"
            [ testVerb "פעל" Hufal Past Male Singular First "הופעלתי"
            , testVerb "פעל" Hufal Present Male Singular First "מופעל"
            , testVerb "פעל" Hufal Future Male Singular First "אופעל"
            , testVerb "פעל" Hufal Past Female Singular First "הופעלתי"
            , testVerb "פעל" Hufal Present Female Singular First "מופעלת"
            , testVerb "פעל" Hufal Future Female Singular First "אופעל"
            , testVerb "פעל" Hufal Past Male Plural First "הופעלנו"
            , testVerb "פעל" Hufal Present Male Plural First "מופעלים"
            , testVerb "פעל" Hufal Future Male Plural First "נופעל"
            , testVerb "פעל" Hufal Past Female Plural First "הופעלנו"
            , testVerb "פעל" Hufal Present Female Plural First "מופעלות"
            , testVerb "פעל" Hufal Future Female Plural First "נופעל"
            , testVerb "פעל" Hufal Past Male Singular Second "הופעלת"
            , testVerb "פעל" Hufal Present Male Singular Second "מופעל"
            , testVerb "פעל" Hufal Future Male Singular Second "תופעל"
            , testVerb "פעל" Hufal Past Female Singular Second "הופעלת"
            , testVerb "פעל" Hufal Present Female Singular Second "מופעלת"
            , testVerb "פעל" Hufal Future Female Singular Second "תופעלי"
            , testVerb "פעל" Hufal Past Male Plural Second "הופעלתם"
            , testVerb "פעל" Hufal Present Male Plural Second "מופעלים"
            , testVerb "פעל" Hufal Future Male Plural Second "תופעלו"
            , testVerb "פעל" Hufal Past Female Plural Second "הופעלתן"
            , testVerb "פעל" Hufal Present Female Plural Second "מופעלות"
            , testVerb "פעל" Hufal Future Female Plural Second "תופעלנה"
            , testVerb "פעל" Hufal Past Male Singular Third "הופעל"
            , testVerb "פעל" Hufal Present Male Singular Third "מופעל"
            , testVerb "פעל" Hufal Future Male Singular Third "יופעל"
            , testVerb "פעל" Hufal Past Female Singular Third "הופעלה"
            , testVerb "פעל" Hufal Present Female Singular Third "מופעלת"
            , testVerb "פעל" Hufal Future Female Singular Third "תופעל"
            , testVerb "פעל" Hufal Past Male Plural Third "הופעלו"
            , testVerb "פעל" Hufal Present Male Plural Third "מופעלים"
            , testVerb "פעל" Hufal Future Male Plural Third "יופעלו"
            , testVerb "פעל" Hufal Past Female Plural Third "הופעלו"
            , testVerb "פעל" Hufal Present Female Plural Third "מופעלות"
            , testVerb "פעל" Hufal Future Female Plural Third "תופעלנה"
            ]
        , describe "פיעל"
            [ testVerb "פעל" Piel Past Male Singular First "פיעלתי"
            , testVerb "פעל" Piel Present Male Singular First "מפעל"
            , testVerb "פעל" Piel Future Male Singular First "אפעל"
            , testVerb "פעל" Piel Past Female Singular First "פיעלתי"
            , testVerb "פעל" Piel Present Female Singular First "מפעלת"
            , testVerb "פעל" Piel Future Female Singular First "אפעל"
            , testVerb "פעל" Piel Past Male Plural First "פיעלנו"
            , testVerb "פעל" Piel Present Male Plural First "מפעלים"
            , testVerb "פעל" Piel Future Male Plural First "נפעל"
            , testVerb "פעל" Piel Past Female Plural First "פיעלנו"
            , testVerb "פעל" Piel Present Female Plural First "מפעלות"
            , testVerb "פעל" Piel Future Female Plural First "נפעל"
            , testVerb "פעל" Piel Past Male Singular Second "פיעלת"
            , testVerb "פעל" Piel Present Male Singular Second "מפעל"
            , testVerb "פעל" Piel Future Male Singular Second "תפעל"
            , testVerb "פעל" Piel Past Female Singular Second "פיעלת"
            , testVerb "פעל" Piel Present Female Singular Second "מפעלת"
            , testVerb "פעל" Piel Future Female Singular Second "תפעלי"
            , testVerb "פעל" Piel Past Male Plural Second "פיעלתם"
            , testVerb "פעל" Piel Present Male Plural Second "מפעלים"
            , testVerb "פעל" Piel Future Male Plural Second "תפעלו"
            , testVerb "פעל" Piel Past Female Plural Second "פיעלתן"
            , testVerb "פעל" Piel Present Female Plural Second "מפעלות"
            , testVerb "פעל" Piel Future Female Plural Second "תפעלנה"
            , testVerb "פעל" Piel Past Male Singular Third "פיעל"
            , testVerb "פעל" Piel Present Male Singular Third "מפעל"
            , testVerb "פעל" Piel Future Male Singular Third "יפעל"
            , testVerb "פעל" Piel Past Female Singular Third "פיעלה"
            , testVerb "פעל" Piel Present Female Singular Third "מפעלת"
            , testVerb "פעל" Piel Future Female Singular Third "תפעל"
            , testVerb "פעל" Piel Past Male Plural Third "פיעלו"
            , testVerb "פעל" Piel Present Male Plural Third "מפעלים"
            , testVerb "פעל" Piel Future Male Plural Third "יפעלו"
            , testVerb "פעל" Piel Past Female Plural Third "פיעלו"
            , testVerb "פעל" Piel Present Female Plural Third "מפעלות"
            , testVerb "פעל" Piel Future Female Plural Third "תפעלנה"
            , testVerb "פעל" Piel Imperative Male Singular First "פעל"
            , testVerb "פעל" Piel Imperative Female Singular First "פעלי"
            , testVerb "פעל" Piel Imperative Male Plural First "פעלו"
            , testVerb "פעל" Piel Imperative Female Plural First "פעלנה"
            ]
        , describe "פועל"
            [ testVerb "פעל" Pual Past Male Singular First "פועלתי"
            , testVerb "פעל" Pual Present Male Singular First "מפועל"
            , testVerb "פעל" Pual Future Male Singular First "אפועל"
            , testVerb "פעל" Pual Past Female Singular First "פועלתי"
            , testVerb "פעל" Pual Present Female Singular First "מפועלת"
            , testVerb "פעל" Pual Future Female Singular First "אפועל"
            , testVerb "פעל" Pual Past Male Plural First "פועלנו"
            , testVerb "פעל" Pual Present Male Plural First "מפועלים"
            , testVerb "פעל" Pual Future Male Plural First "נפועל"
            , testVerb "פעל" Pual Past Female Plural First "פועלנו"
            , testVerb "פעל" Pual Present Female Plural First "מפועלות"
            , testVerb "פעל" Pual Future Female Plural First "נפועל"
            , testVerb "פעל" Pual Past Male Singular Second "פועלת"
            , testVerb "פעל" Pual Present Male Singular Second "מפועל"
            , testVerb "פעל" Pual Future Male Singular Second "תפועל"
            , testVerb "פעל" Pual Past Female Singular Second "פועלת"
            , testVerb "פעל" Pual Present Female Singular Second "מפועלת"
            , testVerb "פעל" Pual Future Female Singular Second "תפועלי"
            , testVerb "פעל" Pual Past Male Plural Second "פועלתם"
            , testVerb "פעל" Pual Present Male Plural Second "מפועלים"
            , testVerb "פעל" Pual Future Male Plural Second "תפועלו"
            , testVerb "פעל" Pual Past Female Plural Second "פועלתן"
            , testVerb "פעל" Pual Present Female Plural Second "מפועלות"
            , testVerb "פעל" Pual Future Female Plural Second "תפועלנה"
            , testVerb "פעל" Pual Past Male Singular Third "פועל"
            , testVerb "פעל" Pual Present Male Singular Third "מפועל"
            , testVerb "פעל" Pual Future Male Singular Third "יפועל"
            , testVerb "פעל" Pual Past Female Singular Third "פועלה"
            , testVerb "פעל" Pual Present Female Singular Third "מפועלת"
            , testVerb "פעל" Pual Future Female Singular Third "תפועל"
            , testVerb "פעל" Pual Past Male Plural Third "פועלו"
            , testVerb "פעל" Pual Present Male Plural Third "מפועלים"
            , testVerb "פעל" Pual Future Male Plural Third "יפועלו"
            , testVerb "פעל" Pual Past Female Plural Third "פועלו"
            , testVerb "פעל" Pual Present Female Plural Third "מפועלות"
            , testVerb "פעל" Pual Future Female Plural Third "תפועלנה"
            ]
        , describe "התפעל"
            [ testVerb "פעל" Hitpael Past Male Singular First "התפעלתי"
            , testVerb "פעל" Hitpael Present Male Singular First "מתפעל"
            , testVerb "פעל" Hitpael Future Male Singular First "אתפעל"
            , testVerb "פעל" Hitpael Past Female Singular First "התפעלתי"
            , testVerb "פעל" Hitpael Present Female Singular First "מתפעלת"
            , testVerb "פעל" Hitpael Future Female Singular First "אתפעל"
            , testVerb "פעל" Hitpael Past Male Plural First "התפעלנו"
            , testVerb "פעל" Hitpael Present Male Plural First "מתפעלים"
            , testVerb "פעל" Hitpael Future Male Plural First "נתפעל"
            , testVerb "פעל" Hitpael Past Female Plural First "התפעלנו"
            , testVerb "פעל" Hitpael Present Female Plural First "מתפעלות"
            , testVerb "פעל" Hitpael Future Female Plural First "נתפעל"
            , testVerb "פעל" Hitpael Past Male Singular Second "התפעלת"
            , testVerb "פעל" Hitpael Present Male Singular Second "מתפעל"
            , testVerb "פעל" Hitpael Future Male Singular Second "תתפעל"
            , testVerb "פעל" Hitpael Past Female Singular Second "התפעלת"
            , testVerb "פעל" Hitpael Present Female Singular Second "מתפעלת"
            , testVerb "פעל" Hitpael Future Female Singular Second "תתפעלי"
            , testVerb "פעל" Hitpael Past Male Plural Second "התפעלתם"
            , testVerb "פעל" Hitpael Present Male Plural Second "מתפעלים"
            , testVerb "פעל" Hitpael Future Male Plural Second "תתפעלו"
            , testVerb "פעל" Hitpael Past Female Plural Second "התפעלתן"
            , testVerb "פעל" Hitpael Present Female Plural Second "מתפעלות"
            , testVerb "פעל" Hitpael Future Female Plural Second "תתפעלנה"
            , testVerb "פעל" Hitpael Past Male Singular Third "התפעל"
            , testVerb "פעל" Hitpael Present Male Singular Third "מתפעל"
            , testVerb "פעל" Hitpael Future Male Singular Third "יתפעל"
            , testVerb "פעל" Hitpael Past Female Singular Third "התפעלה"
            , testVerb "פעל" Hitpael Present Female Singular Third "מתפעלת"
            , testVerb "פעל" Hitpael Future Female Singular Third "תתפעל"
            , testVerb "פעל" Hitpael Past Male Plural Third "התפעלו"
            , testVerb "פעל" Hitpael Present Male Plural Third "מתפעלים"
            , testVerb "פעל" Hitpael Future Male Plural Third "יתפעלו"
            , testVerb "פעל" Hitpael Past Female Plural Third "התפעלו"
            , testVerb "פעל" Hitpael Present Female Plural Third "מתפעלות"
            , testVerb "פעל" Hitpael Future Female Plural Third "תתפעלנה"
            , testVerb "פעל" Hitpael Imperative Male Singular First "התפעל"
            , testVerb "פעל" Hitpael Imperative Female Singular First "התפעלי"
            , testVerb "פעל" Hitpael Imperative Male Plural First "התפעלו"
            , testVerb "פעל" Hitpael Imperative Female Plural First "התפעלנה"
            ]
        ]


main : Test.Runner.Html.TestProgram
main =
    Test.Runner.Html.run suite
