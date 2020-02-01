module Main exposing (Model, Msg, init, main, update, view)

import Browser
import Debouncer exposing (DebouncedInput)
import Html exposing (Html, div, input, label, p, text)
import Html.Attributes exposing (for, id, value)
import Html.Events exposing (onInput)
import Http exposing (Error)
import Json.Decode exposing (Decoder, field, int, list, map2, string)



-- APP


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


initialModel : Model
initialModel =
    { rhymes = NotAsked
    , phrases = NotAsked
    , debouncedInput = Debouncer.initial
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , getPhrases
    )



-- MODEL


type alias Model =
    { rhymes : RhymeResult
    , phrases : PhraseResult
    , debouncedInput : DebouncedInput
    }


type RemoteData a
    = NotAsked
    | Loading
    | Failure
    | Success a


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



-- UPDATE


type Msg
    = GotRhymes (Result Error (List Rhyme))
    | GotPhrases (Result Error String)
    | WordInput String
    | FinishedWord String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRhymes result ->
            case result of
                Ok rhymes ->
                    ( { model | rhymes = Success (goodRhymes rhymes) }, Cmd.none )

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

        WordInput word ->
            let
                ( debouncer, cmd ) =
                    Debouncer.debounce word model.debouncedInput FinishedWord
            in
            ( { model | debouncedInput = debouncer, rhymes = NotAsked }, cmd )

        FinishedWord word ->
            let
                debouncer =
                    Debouncer.updateDelayed word model.debouncedInput
            in
            if Debouncer.ready debouncer then
                ( { model | debouncedInput = debouncer, rhymes = Loading }
                , getRhymes <| word
                )

            else
                ( { model | debouncedInput = debouncer }, Cmd.none )


goodRhymes : List Rhyme -> List Rhyme
goodRhymes rhymes =
    List.filter goodRhyme rhymes


goodRhyme : Rhyme -> Bool
goodRhyme rhyme =
    rhyme.score >= 300


punList : Model -> List Pun
punList model =
    let
        phrases =
            phraseResultToPhraseList model.phrases

        rhymes =
            rhymeList model.rhymes
    in
    List.filter (hasMatchingRhymes rhymes) phrases
        |> List.map
            (rhymesAndWordMakePhrasePunchline rhymes <| Debouncer.changedValue model.debouncedInput)
        |> List.map Pun


hasMatchingRhymes : List Rhyme -> Phrase -> Bool
hasMatchingRhymes rhymes phrase =
    let
        (Phrase phraseString) =
            phrase
    in
    List.map (phraseContainsMatchingRhyme phraseString) rhymes
        |> List.member True


rhymesAndWordMakePhrasePunchline : List Rhyme -> String -> Phrase -> String
rhymesAndWordMakePhrasePunchline rhymes word phrase =
    let
        (Phrase phraseString) =
            phrase
    in
    phraseString
        ++ "... more like: "
        ++ String.replace
            (possibleRhyme phrase rhymes)
            word
            phraseString


possibleRhyme : Phrase -> List Rhyme -> String
possibleRhyme (Phrase phrase) rhymes =
    case matchingRhyme phrase rhymes of
        Just rhyme ->
            rhyme.word

        Nothing ->
            ""


matchingRhyme : String -> List Rhyme -> Maybe Rhyme
matchingRhyme phrase rhymes =
    List.filter (phraseContainsMatchingRhyme phrase) rhymes
        |> List.head


phraseContainsMatchingRhyme : String -> Rhyme -> Bool
phraseContainsMatchingRhyme phrase rhyme =
    List.member rhyme.word (String.words phrase)


rhymeList : RhymeResult -> List Rhyme
rhymeList rhymeResult =
    case rhymeResult of
        Success rhyme ->
            rhyme

        _ ->
            []


phraseResultToPhraseList : PhraseResult -> List Phrase
phraseResultToPhraseList result =
    case result of
        Success phrases ->
            phrases

        _ ->
            []



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ inputView model
        , punView model
        ]


punView : Model -> Html Msg
punView model =
    case model.phrases of
        NotAsked ->
            text "Type a word above for puns"

        Failure ->
            text "It failed :("

        Loading ->
            text "Loading..."

        Success _ ->
            case model.rhymes of
                NotAsked ->
                    if Debouncer.isWaiting model.debouncedInput then
                        text "Waiting for typing to complete before puns are generated"

                    else
                        text "Type a word above for puns"

                Failure ->
                    text "It failed :("

                Loading ->
                    text "Loading..."

                Success _ ->
                    case punList model of
                        [] ->
                            text "No puns :("

                        _ ->
                            punParagraphs (punList model) |> div []


punParagraphs : List Pun -> List (Html Msg)
punParagraphs puns =
    List.map punToString puns
        |> List.map text
        |> List.map List.singleton
        |> List.map (p [])


punToString : Pun -> String
punToString (Pun pun) =
    pun


inputView : Model -> Html Msg
inputView model =
    div []
        [ label [ for "rhyme-word" ] [ text "Word: " ]
        , input [ id "rhyme-word", value <| Debouncer.changedValue model.debouncedInput, onInput WordInput ] []
        ]



-- COMMANDS


getPhrases : Cmd Msg
getPhrases =
    Http.get
        { url = "/data/wikipedia-idioms.txt"
        , expect = Http.expectString GotPhrases
        }


getRhymes : String -> Cmd Msg
getRhymes word =
    Http.get
        -- uncomment below if you want to real rhyme data
        { url = "https://rhymebrain.com/talk?function=getRhymes&word=" ++ word

        -- uncomment below if you want to use pre-downloaded rhyme data
        -- { url = "/data/" ++ word ++ "-rhymes.json"
        , expect = Http.expectJson GotRhymes rhymesDecoder
        }


rhymesDecoder : Decoder (List Rhyme)
rhymesDecoder =
    list rhymeDecoder


rhymeDecoder : Decoder Rhyme
rhymeDecoder =
    map2 Rhyme (field "word" string) (field "score" int)
