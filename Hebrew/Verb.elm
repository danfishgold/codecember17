module Hebrew.Verb exposing (..)


verb : List String -> Verb
verb root =
    { root = root
    , conjugation = Paal
    , tense = Past
    , person = First
    , sex = Male
    }


type alias Verb =
    { root : List String
    , conjugation : Conjugation
    , tense : Tense
    , person : Person
    , sex : Sex
    }


type Conjugation
    = Paal
    | Nifal
    | Hifil
    | Hufal
    | Piel
    | Pual
    | Hitpael


type Tense
    = Past
    | Present
    | Future
    | Imperative


type Person
    = First
    | Second
    | Third


type Sex
    = Male
    | Female
