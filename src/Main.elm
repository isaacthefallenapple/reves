module Main exposing (main)

import Abilities exposing (Abilities)
import Ability exposing (Ability)
import Boon
import Browser
import Browser.Navigation as Nav
import Character exposing (Msg(..))
import Class
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Ports
import Task
import Url



-- TYPES


type Model
    = PickClass
    | PickAssignment Class.Class
    | Character Character.Stats
    | DecodeErr Decode.Error
    | Abilities Abilities



-- MSG


type Msg
    = PickedClass Class.Class
    | PickedAssignment Boon.Assignment
    | UpdatedCharacter Character.Msg
    | ClickedOpenFile
    | FileLoaded File
    | ReadFile String
    | RequestedAbilities (Result Http.Error Abilities.Advances)
    | ClickedSave
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url



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

        Abilities abilities ->
            { title = abilities.title
            , body = [ Abilities.view abilities ]
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
            ( Character character
            , saveCharacter character
            )

        ( Character character, UpdatedCharacter (Updated m) ) ->
            let
                updatedCharacter =
                    Character.update m character
            in
            ( Character updatedCharacter, saveCharacter updatedCharacter )

        ( Character character, UpdatedCharacter ClickedViewAbilities ) ->
            Tuple.mapFirst Abilities (Abilities.init RequestedAbilities character)

        ( Abilities abilities, RequestedAbilities response ) ->
            ( Abilities (Abilities.update response abilities), Cmd.none )

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
