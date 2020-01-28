module Main exposing (Model, Msg, init, main, subscriptions, update, view)

import Browser
import Html exposing (Html, div, p, text)
import Http exposing (Error)
import Json.Decode exposing (Decoder, field, int, list, map2, string)



-- APP


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


initialModel : Model
initialModel =
    { rhymes = Loading, phrases = Loading, word = "code" }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.batch [ getRhymes initialModel.word, getPhrases ]
    )



-- MODEL


type alias Model =
    { rhymes : RhymeResult
    , phrases : PhraseResult
    , word : String
    }


type RemoteData a
    = Loading
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
            (rhymesAndWordMakePhrasePunchline rhymes model.word)
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


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
                    punView (punList model)


punView : List Pun -> Html Msg
punView puns =
    case puns of
        [] ->
            text "No puns :("

        _ ->
            punParagraphs puns |> div []


punParagraphs : List Pun -> List (Html Msg)
punParagraphs puns =
    List.map punToString puns
        |> List.map text
        |> List.map List.singleton
        |> List.map (p [])


punToString : Pun -> String
punToString (Pun pun) =
    pun



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
        { url = "https://rhymebrain.com/talk?function=getRhymes&word=" ++ word
        , expect = Http.expectJson GotRhymes rhymesDecoder
        }


rhymesDecoder : Decoder (List Rhyme)
rhymesDecoder =
    list rhymeDecoder


rhymeDecoder : Decoder Rhyme
rhymeDecoder =
    map2 Rhyme (field "word" string) (field "score" int)
