module Main exposing (main)

import Browser
import Character
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe
import Ports
import Task



-- TYPES


type Model
    = Character Character.Stats
    | DecodeErr Decode.Error



-- MSG


type Msg
    = UpdatedCharacter Character.Stats
    | ClickedOpenFile
    | FileLoaded File
    | ReadFile String
    | ClickedSave



-- ENCODE


saveCharacter : Character.Stats -> Cmd msg
saveCharacter character =
    Encode.encode 2 (Character.encode character) |> Ports.storeCharacter



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model of
        DecodeErr err ->
            { title = "Error"
            , body =
                [ h1
                    []
                    [ text "Something went wrong :("
                    ]
                , p
                    []
                    [ text (Decode.errorToString err) ]
                , p
                    []
                    [ button
                        [ onClick ClickedOpenFile ]
                        [ text "Try a different file" ]
                    ]
                ]
            }

        Character character ->
            Character.view UpdatedCharacter character



-- UPDATE
-- updateCharacter : ( Stats, Cmd Msg ) -> ( Model, Cmd Msg )
-- updateCharacter ( character, msg ) =
--     ( Character character, Cmd.batch [ msg, saveCharacter character ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Character _, UpdatedCharacter updatedCharacter ) ->
            ( Character updatedCharacter, saveCharacter updatedCharacter )

        ( Character character, ClickedSave ) ->
            ( model
            , Download.string character.name
                "application/json"
                (Encode.encode 2 (Character.encode character))
            )

        ( _, ClickedOpenFile ) ->
            ( model, Select.file [ "application/json" ] FileLoaded )

        ( _, FileLoaded file ) ->
            ( model, Task.perform ReadFile (File.toString file) )

        ( _, ReadFile content ) ->
            ( case Decode.decodeString Character.decoder content of
                Err err ->
                    DecodeErr err

                Ok character ->
                    Character character
            , Cmd.none
            )

        ( _, _ ) ->
            ( model, Cmd.none )



-- INIT


init : Maybe String -> ( Model, Cmd Msg )
init flags =
    ( Character (Maybe.withDefault Character.blank (Maybe.map Character.decodeLocalCharacter flags))
    , Cmd.none
    )



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MAIN


main : Program (Maybe String) Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
