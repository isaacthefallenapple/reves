module Main exposing (main)

import Boon
import Browser
import Character
import Class
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Ports
import Task



-- TYPES


type Model
    = PickClass
    | PickAssignment Class.Class
    | Character Character.Stats
    | DecodeErr Decode.Error



-- MSG


type Msg
    = PickedClass Class.Class
    | PickedAssignment Boon.Assignment
    | UpdatedCharacter Character.Stats
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

        PickClass ->
            { title = "Pick a class"
            , body =
                [ div
                    [ class "class-picker" ]
                    (List.map
                        (\class_ ->
                            button
                                [ onClick (PickedClass class_) ]
                                [ text class_.name ]
                        )
                        Class.classes
                    )
                ]
            }

        PickAssignment _ ->
            { title = "Pick an assignment"
            , body =
                [ div
                    [ class "assignment-picker" ]
                    (List.map
                        (\assignment ->
                            button
                                [ onClick (PickedAssignment assignment) ]
                                [ text assignment.name ]
                        )
                        Boon.assignments
                    )
                ]
            }

        Character character ->
            let
                characterView =
                    Character.view UpdatedCharacter character
            in
            { title = characterView.title
            , body =
                characterView.body
            }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( PickClass, PickedClass class ) ->
            ( PickAssignment class, Cmd.none )

        ( PickAssignment class, PickedAssignment assignment ) ->
            let
                character =
                    Character.blank
                        |> Character.applyClass class
                        |> Character.applyAssignment assignment
            in
            ( Character
                character
            , saveCharacter character
            )

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
    ( case flags of
        Nothing ->
            PickClass

        Just json ->
            Character (Character.decodeLocalCharacter json)
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
