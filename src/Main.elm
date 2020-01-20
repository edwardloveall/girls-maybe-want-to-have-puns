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


type Pun
    = Pun String


type alias PhraseResult =
    RemoteData (List Phrase)


type alias Model =
    { rhymes : RhymeResult
    , phrases : PhraseResult
    , puns : List Pun
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
    ( Model Loading Loading [] "heart", Cmd.batch [ getRhymes "heart", getPhrases ] )


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
                    ( { model
                        | phrases =
                            String.lines phrases
                                |> List.map Phrase
                                |> Success
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | phrases = Failure }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    punView model


punView : Model -> Html Msg
punView model =
    case model.rhymes of
        Failure ->
            text "It failed :("

        Loading ->
            text "Loading..."

        Success rhymes ->
            case model.phrases of
                Failure ->
                    text "It failed :("

                Loading ->
                    text "Loading..."

                Success phrases ->
                    punList (goodRhymes rhymes) phrases


punList : List Rhyme -> List Phrase -> Html Msg
punList rhymes phrases =
    List.filter (\phrase -> hasMatchingRhymes phrase rhymes)
        phrases
        |> List.map
            (\phrase -> phraseToString phrase)
        |> List.map (\phrase -> p [] [ text phrase ])
        |> div []


hasMatchingRhymes : Phrase -> List Rhyme -> Bool
hasMatchingRhymes phrase rhymes =
    List.map
        (\rhyme -> phraseContainsMatchingWord (phraseToString phrase) rhyme.word)
        rhymes
        |> List.any isTrue


goodRhymes : List Rhyme -> List Rhyme
goodRhymes rhymes =
    List.filter goodRhyme rhymes


goodRhyme : Rhyme -> Bool
goodRhyme rhyme =
    rhyme.score >= 300


phraseContainsMatchingWord : String -> String -> Bool
phraseContainsMatchingWord phrase word =
    List.any (equals word) (String.words phrase)


equals : a -> a -> Bool
equals first second =
    if first == second then
        True

    else
        False


isTrue : Bool -> Bool
isTrue =
    equals True


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
