module Debouncer exposing
    ( DebouncedInput
    , changedValue
    , debounce
    , finishTyping
    , initial
    , isWaiting
    , ready
    , updateChanged
    , updateDelayed
    )

import Process
import Task


type DebouncedInput
    = Input String String


initial : DebouncedInput
initial =
    Input "" ""


changedValue : DebouncedInput -> String
changedValue debouncer =
    case debouncer of
        Input changed _ ->
            changed


updateChanged : String -> DebouncedInput -> DebouncedInput
updateChanged newValue debouncer =
    case debouncer of
        Input _ delayed ->
            Input newValue delayed


updateDelayed : String -> DebouncedInput -> DebouncedInput
updateDelayed newValue debouncer =
    case debouncer of
        Input changed _ ->
            Input changed newValue


finishTyping : DebouncedInput -> DebouncedInput
finishTyping debouncer =
    case debouncer of
        Input changed delayed ->
            Input changed delayed


debounce : String -> DebouncedInput -> (String -> a) -> ( DebouncedInput, Cmd a )
debounce changed debouncer msg =
    let
        newDebouncer =
            updateChanged changed debouncer
    in
    ( newDebouncer
    , Process.sleep 750
        |> Task.perform (\_ -> msg <| changed)
    )


ready : DebouncedInput -> Bool
ready debouncer =
    case debouncer of
        Input changed delayed ->
            changed == delayed


isWaiting : DebouncedInput -> Bool
isWaiting debouncer =
    ready debouncer /= True
