module Main exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Html exposing (Html, div, text)
import Http exposing (Error)
import Json.Decode exposing (Decoder, field, list, string)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Pun =
    String


type Model
    = Failure
    | Loading
    | Success (List Pun)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getPuns )


type Msg
    = GotPuns (Result Error (List Pun))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        GotPuns result ->
            case result of
                Ok puns ->
                    ( Success puns, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    viewPuns model


viewPuns : Model -> Html Msg
viewPuns model =
    case model of
        Failure ->
            text "It failed"

        Loading ->
            text "Loading..."

        Success puns ->
            div []
                (List.map
                    text
                    puns
                )


getPuns : Cmd Msg
getPuns =
    Http.get
        { url = "http://rhymebrain.com/talk?function=getRhymes&word=heart"
        , expect = Http.expectJson GotPuns punDecoder
        }


punDecoder : Decoder (List Pun)
punDecoder =
    list (field "word" string)
