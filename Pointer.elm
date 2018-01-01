module Pointer
    exposing
        ( Point
        , inCollage
        , mouseDown
        , mouseUp
        , mouseMove
        , click
        , touchStart
        , touchMove
        , touchEnd
        , touchCancel
        )

import Json.Decode as Json
import Html.Events exposing (on, onWithOptions, defaultOptions)
import Html exposing (Html, Attribute)


--


type alias Point =
    ( Float, Float )


mousePoint : Json.Decoder Point
mousePoint =
    point


point : Json.Decoder Point
point =
    Json.map2 (,)
        (Json.field "offsetX" Json.float)
        (Json.field "offsetY" Json.float)


touchPoint : Json.Decoder Point
touchPoint =
    Json.at [ "changedTouches", "0" ] clientPoint


clientPoint : Json.Decoder Point
clientPoint =
    Json.map2 (,)
        (Json.field "clientX" Json.float)
        (Json.field "clientY" Json.float)


inCollage : { a | width : Float, height : Float } -> Point -> Point
inCollage { width, height } ( x, y ) =
    ( x - width / 2, height / 2 - y )


click : (Point -> msg) -> Attribute msg
click msg =
    on "click" (mousePoint |> Json.map msg)


mouseDown : (Point -> msg) -> Attribute msg
mouseDown msg =
    on "mousedown" (mousePoint |> Json.map msg)


mouseUp : (Point -> msg) -> Attribute msg
mouseUp msg =
    on "mouseup" (mousePoint |> Json.map msg)


mouseMove : (Point -> msg) -> Attribute msg
mouseMove msg =
    on "mousemove" (mousePoint |> Json.map msg)


touchOptions : Html.Events.Options
touchOptions =
    { stopPropagation = True, preventDefault = True }


touchStart : (Point -> msg) -> Attribute msg
touchStart msg =
    onWithOptions "touchstart"
        touchOptions
        (Json.map msg touchPoint)


touchMove : (Point -> msg) -> Attribute msg
touchMove msg =
    onWithOptions "touchmove"
        touchOptions
        (Json.map msg touchPoint)


touchEnd : (Point -> msg) -> Attribute msg
touchEnd msg =
    onWithOptions "touchend"
        touchOptions
        (Json.map msg touchPoint)


touchCancel : (Point -> msg) -> Attribute msg
touchCancel msg =
    onWithOptions "touchcancel"
        touchOptions
        (Json.map msg touchPoint)
