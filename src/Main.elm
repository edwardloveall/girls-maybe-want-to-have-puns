module Main exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Html exposing (Html, div, p, text)
import Http exposing (Error)
import Json.Decode exposing (Decoder, field, int, list, map2, string)


type RemoteData a
    = Loading
    | Failure
    | Success a


type alias Rhyme =
    { word : String
    , score : Int
    }


type alias RhymeResult =
    RemoteData (List Rhyme)


type Phrase
    = Phrase String


type alias PhraseResult =
    RemoteData (List Phrase)


type alias Model =
    { rhymes : RhymeResult
    , phrases : PhraseResult
    }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Loading Loading, Cmd.batch [ getRhymes, getPhrases ] )


type Msg
    = GotRhymes (Result Error (List Rhyme))
    | GotPhrases (Result Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRhymes result ->
            case result of
                Ok rhymes ->
                    ( { model | rhymes = Success rhymes }, Cmd.none )

                Err _ ->
                    ( { model | rhymes = Failure }, Cmd.none )

        GotPhrases result ->
            case result of
                Ok phrases ->
                    ( { model | phrases = String.split "\n" phrases |> List.map Phrase |> Success }, Cmd.none )

                Err _ ->
                    ( { model | phrases = Failure }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    viewPuns model


viewPuns : Model -> Html Msg
viewPuns model =
    div []
        [ rhymeView model.rhymes, phraseView model.phrases ]


rhymeView : RhymeResult -> Html Msg
rhymeView rhymeResult =
    case rhymeResult of
        Failure ->
            text "It failed"

        Loading ->
            text "Loading..."

        Success rhymes ->
            rhymeList
                rhymes
                |> div []


phraseView : PhraseResult -> Html Msg
phraseView phraseResult =
    case phraseResult of
        Failure ->
            text "It failed"

        Loading ->
            text "Loading..."

        Success phrases ->
            List.map phraseToParagraph
                phrases
                |> div []


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


phraseToParagraph : Phrase -> Html Msg
phraseToParagraph phrase =
    let
        (Phrase magic) =
            phrase
    in
    p [] [ text magic ]


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


getPhrases : Cmd Msg
getPhrases =
    Http.get
        { url = "/data/wikipedia-idioms.txt"
        , expect = Http.expectString GotPhrases
        }
