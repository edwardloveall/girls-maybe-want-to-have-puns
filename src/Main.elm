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
    , word : String
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
    ( Model Loading Loading "heart", Cmd.batch [ getRhymes "heart", getPhrases ] )


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
                    ( { model | phrases = String.lines phrases |> List.map Phrase |> Success }, Cmd.none )

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
        [ rhymeView model.rhymes, phraseView model ]


rhymeView : RhymeResult -> Html Msg
rhymeView rhymeResult =
    case rhymeResult of
        Failure ->
            text "It failed"

        Loading ->
            text "Loading..."

        Success rhymes ->
            rhymeList rhymes


phraseView : Model -> Html Msg
phraseView model =
    case model.phrases of
        Failure ->
            text "It failed"

        Loading ->
            text "Loading..."

        Success phrases ->
            phraseList phrases model.word


rhymeList : List Rhyme -> Html Msg
rhymeList rhymes =
    List.map
        rhymeToParagraph
        (goodRhymes
            rhymes
        )
        |> div []


goodRhymes : List Rhyme -> List Rhyme
goodRhymes rhymes =
    List.filter goodRhyme rhymes


goodRhyme : Rhyme -> Bool
goodRhyme rhyme =
    rhyme.score >= 300


rhymeToParagraph : Rhyme -> Html Msg
rhymeToParagraph rhyme =
    p [] [ text rhyme.word ]


phraseList : List Phrase -> String -> Html Msg
phraseList phrases word =
    List.map
        (\phrase -> phraseToString phrase)
        phrases
        |> List.filter (\phrase -> containsMatch phrase word)
        |> List.map (\phrase -> p [] [ text phrase ])
        |> div []


containsMatch : String -> String -> Bool
containsMatch phrase word =
    List.any (equals word) (String.words phrase)


equals : a -> a -> Bool
equals first second =
    if first == second then
        True

    else
        False


phraseToString : Phrase -> String
phraseToString phrase =
    let
        (Phrase string) =
            phrase
    in
    string


getRhymes : String -> Cmd Msg
getRhymes word =
    Http.get
        { url = "https://rhymebrain.com/talk?function=getRhymes&word=" ++ word
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
