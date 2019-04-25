port module ElementSize exposing (changes, get)

import Util exposing (Size)


port getSize : () -> Cmd msg


port sizeChanges : (Size -> msg) -> Sub msg


get : Cmd msg
get =
    getSize ()


changes : (Size -> msg) -> Sub msg
changes =
    sizeChanges
