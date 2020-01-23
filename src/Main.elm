module Main exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Html exposing (Html, div, p, text)
import Http exposing (Error)
import Json.Decode exposing (Decoder, field, int, list, map2, string)


type RemoteData a
    = Loading
    | Failure
    | Success a


type PunData a
    = NoPuns
    | JustPuns a


type alias Rhyme =
    { word : String
    , score : Int
    }


type Phrase
    = Phrase String


type alias RhymeResult =
    RemoteData (List Rhyme)


type alias PhraseResult =
    RemoteData (List Phrase)


type Pun
    = Pun String


type alias Model =
    { rhymes : RhymeResult
    , phrases : PhraseResult
    , puns : PunData (List Pun)
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
    ( Model Loading Loading NoPuns "code", Cmd.batch [ getRhymes "code", getPhrases ] )


type Msg
    = GotRhymes (Result Error (List Rhyme))
    | GotPhrases (Result Error String)
    | SetPuns Model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRhymes result ->
            case result of
                Ok rhymes ->
                    let
                        newModel =
                            { model | rhymes = Success rhymes }
                    in
                    update (SetPuns newModel) newModel

                Err _ ->
                    ( { model | rhymes = Failure }, Cmd.none )

        GotPhrases result ->
            case result of
                Ok phrases ->
                    let
                        newModel =
                            { model
                                | phrases =
                                    String.lines phrases
                                        |> List.map Phrase
                                        |> Success
                            }
                    in
                    update (SetPuns newModel) newModel

                Err _ ->
                    ( { model | phrases = Failure }, Cmd.none )

        SetPuns newModel ->
            ( { newModel | puns = JustPuns (punList model) }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    case model.rhymes of
        Failure ->
            text "It failed :("

        Loading ->
            text "Loading..."

        Success _ ->
            case model.phrases of
                Failure ->
                    text "It failed :("

                Loading ->
                    text "Loading..."

                Success _ ->
                    case model.puns of
                        NoPuns ->
                            text "No puns yet"

                        JustPuns puns ->
                            punView puns


punView : List Pun -> Html Msg
punView puns =
    punParagraphs (punsToStrings puns) |> div []


punParagraphs : List String -> List (Html Msg)
punParagraphs puns =
    List.map (\pun -> p [] [ text pun ]) puns


punWithPunchline : String -> List Rhyme -> String -> String
punWithPunchline phrase rhymes word =
    phrase
        ++ "... more like: "
        ++ String.replace
            (case matchingRhyme phrase rhymes of
                Just rhyme ->
                    rhyme.word

                Nothing ->
                    ""
            )
            word
            phrase


matchingRhyme : String -> List Rhyme -> Maybe Rhyme
matchingRhyme phrase rhymes =
    List.filter (\rhyme -> List.member rhyme.word (String.words phrase)) rhymes
        |> List.head


punList : Model -> List Pun
punList model =
    List.filter
        (\phrase ->
            hasMatchingRhymes phrase
                (goodRhymes (rhymeList model.rhymes))
        )
        (phraseResultToPhraseList
            model.phrases
        )
        |> List.map
            (\phrase -> phraseToString phrase)
        |> List.map
            (\phrase ->
                punWithPunchline
                    phrase
                    (goodRhymes (rhymeList model.rhymes))
                    model.word
            )
        |> List.map (\phrase -> Pun phrase)


hasMatchingRhymes : Phrase -> List Rhyme -> Bool
hasMatchingRhymes phrase rhymes =
    List.map
        (\rhyme -> phraseContainsMatchingWord (phraseToString phrase) rhyme.word)
        rhymes
        |> List.any isTrue


rhymeList : RhymeResult -> List Rhyme
rhymeList rhymeResult =
    case rhymeResult of
        Failure ->
            []

        Loading ->
            []

        Success rhyme ->
            rhyme


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


punsToStrings : List Pun -> List String
punsToStrings puns =
    List.map (\pun -> punToString pun) puns


punToString : Pun -> String
punToString pun =
    let
        (Pun string) =
            pun
    in
    string


phraseResultToPhraseList : PhraseResult -> List Phrase
phraseResultToPhraseList result =
    case result of
        Failure ->
            []

        Loading ->
            []

        Success phrases ->
            phrases


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
