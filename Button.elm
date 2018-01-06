module Button exposing (Button, button, startClick, endClick, view)

import Pointer exposing (Pointer)
import Pointer.Mapping exposing (Mapping)
import Point exposing (Point)
import TextRect
import Types exposing (Size)
import Set exposing (Set)
import Pointer.Id
import Task
import Collage exposing (Collage)
import Color


type alias Button msg =
    { text : String
    , position : Point
    , padding : Size
    , onClick : msg
    , enabled : Bool
    , currentPointers : Set String
    }


button : String -> msg -> Bool -> Button msg
button text onClick enabled =
    { text = text
    , position = ( 0, 0 )
    , padding = TextRect.defaultPadding
    , onClick = onClick
    , enabled = enabled
    , currentPointers = Set.empty
    }


startClick : Mapping Pointer -> List (Button msg) -> ( List (Button msg), Mapping Pointer )
startClick pointers buttons =
    let
        ( newButtons, remainingPointers ) =
            foldReduce collectPointers buttons (Pointer.Mapping.toList pointers)
    in
        ( newButtons, Pointer.Mapping.fromList .id remainingPointers )


endClick : Mapping Pointer -> List (Button msg) -> ( ( List (Button msg), Mapping Pointer ), Cmd msg )
endClick pointers buttons =
    let
        ( newButtonsAndMsgs, remainingPointers ) =
            foldReduce removePointers buttons (Pointer.Mapping.toList pointers)

        ( newButtons, msgs ) =
            List.unzip newButtonsAndMsgs |> Tuple.mapSecond (List.filterMap identity)

        cmd =
            if List.isEmpty msgs then
                Cmd.none
            else
                msgs
                    |> List.map (\msg -> Task.perform identity (Task.succeed msg))
                    |> Cmd.batch
    in
        ( ( newButtons, Pointer.Mapping.fromList .id remainingPointers ), cmd )


collectPointers : Button msg -> List Pointer -> ( Button msg, List Pointer )
collectPointers button pointers =
    let
        ( inside, outside ) =
            List.partition (.position >> flip TextRect.contains button) pointers
    in
        ( { button | currentPointers = Set.fromList <| List.map (.id >> Pointer.Id.toString) inside }, outside )


removePointers : Button msg -> List Pointer -> ( ( Button msg, Maybe msg ), List Pointer )
removePointers button pointers =
    let
        ( relevant, irrelevant ) =
            List.partition (.id >> Pointer.Id.toString >> flip Set.member button.currentPointers) pointers

        ( inside, outside ) =
            List.partition (.position >> flip TextRect.contains button) relevant
    in
        if List.isEmpty inside then
            ( ( button, Nothing ), irrelevant ++ outside )
        else
            ( ( button, Just button.onClick ), irrelevant ++ outside )


foldReduce : (a -> List b -> ( c, List b )) -> List a -> List b -> ( List c, List b )
foldReduce fn xs ys =
    let
        folder a ( prevZs, remainingYs ) =
            fn a remainingYs |> Tuple.mapFirst (\newZ -> newZ :: prevZs)
    in
        List.foldl folder ( [], ys ) xs |> Tuple.mapFirst List.reverse


view : Button msg -> Collage msg
view button =
    if button.enabled then
        TextRect.view Color.white Color.black button
    else
        TextRect.view Color.white Color.gray button
