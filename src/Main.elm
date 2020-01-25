module Main exposing (Model, Msg, init, subscriptions, update, view)

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


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Loading Loading Maybe.Nothing "code"
    , Cmd.batch [ getRhymes "code", getPhrases ]
    )



-- MODEL


type alias Model =
    { rhymes : RhymeResult
    , phrases : PhraseResult
    , puns : Maybe (List Pun)
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
            ( { newModel | puns = Maybe.Just (punList model) }, Cmd.none )


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
            (rhymesAndWordMakePhrasePunchline
                (goodRhymes (rhymeList model.rhymes))
                model.word
            )
        |> List.map Pun


goodRhymes : List Rhyme -> List Rhyme
goodRhymes rhymes =
    List.filter goodRhyme rhymes


goodRhyme : Rhyme -> Bool
goodRhyme rhyme =
    rhyme.score >= 300


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
possibleRhyme phrase rhymes =
    case matchingRhyme (phraseToString phrase) rhymes of
        Just rhyme ->
            rhyme.word

        Nothing ->
            ""


matchingRhyme : String -> List Rhyme -> Maybe Rhyme
matchingRhyme phrase rhymes =
    List.filter (\rhyme -> List.member rhyme.word (String.words phrase)) rhymes
        |> List.head


hasMatchingRhymes : Phrase -> List Rhyme -> Bool
hasMatchingRhymes phrase rhymes =
    let
        (Phrase phraseString) =
            phrase
    in
    List.map .word rhymes
        |> List.map (phraseContainsMatchingWord phraseString)
        |> List.member True


phraseContainsMatchingWord : String -> String -> Bool
phraseContainsMatchingWord phrase word =
    List.member word (String.words phrase)


rhymeList : RhymeResult -> List Rhyme
rhymeList rhymeResult =
    case rhymeResult of
        Failure ->
            []

        Loading ->
            []

        Success rhyme ->
            rhyme


phraseToString : Phrase -> String
phraseToString phrase =
    let
        (Phrase string) =
            phrase
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
                    case model.puns of
                        Nothing ->
                            text "No puns yet"

                        Just puns ->
                            punView puns


punView : List Pun -> Html Msg
punView puns =
    punParagraphs (punsToStrings puns) |> div []


punParagraphs : List String -> List (Html Msg)
punParagraphs puns =
    List.map (\pun -> p [] [ text pun ]) puns


punsToStrings : List Pun -> List String
punsToStrings puns =
    List.map punToString puns


punToString : Pun -> String
punToString pun =
    let
        (Pun string) =
            pun
    in
    string



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
