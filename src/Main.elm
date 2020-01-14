module Main exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Html exposing (Html, div, p, text)
import Http exposing (Error)
import Json.Decode exposing (Decoder, field, int, list, map2, string)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Rhyme =
    { word : String
    , score : Int
    }


type Model
    = Failure
    | Loading
    | Success (List Rhyme)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getRhymes )


type Msg
    = GotRhymes (Result Error (List Rhyme))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        GotRhymes result ->
            case result of
                Ok rhymes ->
                    ( Success rhymes, Cmd.none )

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

        Success rhymes ->
            div []
                (rhymeList
                    rhymes
                )


rhymeList : List Rhyme -> List (Html Msg)
rhymeList rhymes =
    List.map
        rhymeToParagraph
        (goodRhymes
            rhymes
        )


goodRhymes : List Rhyme -> List Rhyme
goodRhymes rhymes =
    List.filter goodRhyme rhymes


goodRhyme : Rhyme -> Bool
goodRhyme rhyme =
    rhyme.score >= 300


rhymeToParagraph : Rhyme -> Html Msg
rhymeToParagraph rhyme =
    p [] [ text rhyme.word ]


getRhymes : Cmd Msg
getRhymes =
    Http.get
        { url = "https://rhymebrain.com/talk?function=getRhymes&word=heart"
        , expect = Http.expectJson GotRhymes rhymesDecoder
        }


rhymesDecoder : Decoder (List Rhyme)
rhymesDecoder =
    list rhymeDecoder


rhymeDecoder : Decoder Rhyme
rhymeDecoder =
    map2 Rhyme (field "word" string) (field "score" int)
