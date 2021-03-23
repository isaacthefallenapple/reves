module Main exposing (main)

import Abilities exposing (Abilities)
import Ability exposing (Ability)
import Array exposing (push)
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
import Route exposing (Route)
import Task
import Url



-- TYPES


type Model
    = PickClass Nav.Key
    | PickAssignment Nav.Key Class.Class
    | Character Nav.Key Character.Stats
    | DecodeErr Nav.Key Decode.Error
    | Abilities Abilities
    | Landing Nav.Key


toNavKey : Model -> Nav.Key
toNavKey model =
    case model of
        PickClass navKey ->
            navKey

        PickAssignment navKey _ ->
            navKey

        Character navKey _ ->
            navKey

        DecodeErr navKey _ ->
            navKey

        Abilities abilities ->
            Abilities.toNavKey abilities

        Landing navKey ->
            navKey



-- MSG


type Msg
    = PickedClass Class.Class
    | PickedAssignment Boon.Assignment
    | CharacterMsg Character.Msg
    | ClickedOpenFile
    | FileLoaded File
    | ReadFile String
    | RequestedAbilities (Result Http.Error Abilities.Advances)
      -- | ClickedSave
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | AbilitiesMsg Abilities.Msg
    | ClickedNewCharacter



-- ENCODE
-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model of
        DecodeErr _ err ->
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

        Landing _ ->
            { title = "Welcome!"
            , body =
                [ h1
                    []
                    [ text "Welcome" ]
                , div
                    []
                    [ button
                        [ onClick ClickedOpenFile ]
                        [ text "Open a character" ]
                    , text " or "
                    , button
                        [ onClick ClickedNewCharacter ]
                        [ text "Create a new one?" ]
                    ]
                ]
            }

        PickClass _ ->
            { title = "Pick a class"
            , body =
                [ div
                    [ class "class-picker" ]
                    (h1
                        []
                        [ text "Pick your class" ]
                        :: List.map
                            (\class_ ->
                                button
                                    [ onClick (PickedClass class_) ]
                                    [ text class_.name ]
                            )
                            Class.classes
                    )
                ]
            }

        PickAssignment _ _ ->
            { title = "Pick an assignment"
            , body =
                [ div
                    [ class "assignment-picker" ]
                    (h1
                        []
                        [ text "Pick your assignment" ]
                        :: List.map
                            (\assignment ->
                                button
                                    [ onClick (PickedAssignment assignment) ]
                                    [ text assignment.name ]
                            )
                            Boon.assignments
                    )
                ]
            }

        Character _ character ->
            let
                characterView =
                    Character.view character
            in
            { title = characterView.title
            , body =
                List.map (Html.map CharacterMsg) characterView.body
            }

        Abilities abilities ->
            { title = abilities.selected
            , body = [ Html.map AbilitiesMsg (Abilities.view abilities) ]
            }



-- UPDATE


wrap : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
wrap toModel toMsg ( subModel, subMsg ) =
    ( toModel subModel, Cmd.map toMsg subMsg )


changeRoute : Route -> Model -> ( Model, Cmd Msg )
changeRoute route model =
    let
        navKey =
            toNavKey model
    in
    case ( model, route ) of
        ( Abilities abilities, Route.Root ) ->
            ( Character navKey abilities.character, Cmd.none )

        ( Character _ character, Route.Abilities selected ) ->
            wrap Abilities AbilitiesMsg (Abilities.init navKey selected character)

        ( PickClass _, Route.Root ) ->
            ( Landing navKey, Cmd.none )

        ( PickAssignment _ _, Route.Root ) ->
            ( Landing navKey, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Landing navKey, ClickedNewCharacter ) ->
            ( PickClass navKey, Cmd.none )

        ( PickClass navKey, PickedClass class ) ->
            ( PickAssignment navKey class, Cmd.none )

        ( PickAssignment navKey class, PickedAssignment assignment ) ->
            let
                character =
                    Character.blank
                        |> Character.applyClass class
                        |> Character.applyAssignment assignment
            in
            ( Character navKey character
            , Character.save character
            )

        ( Character _ character, CharacterMsg ClickedSave ) ->
            ( model
            , Download.string character.name
                "application/json"
                (Encode.encode 2 (Character.encode character))
            )

        ( Character navKey character, CharacterMsg subMsg ) ->
            let
                updatedCharacter =
                    Character.update subMsg character
            in
            ( Character navKey updatedCharacter, Character.save updatedCharacter )

        ( Abilities abilities, AbilitiesMsg subMsg ) ->
            Tuple.mapBoth Abilities (Cmd.map AbilitiesMsg) (Abilities.update subMsg abilities)

        ( _, ClickedOpenFile ) ->
            ( model, Select.file [ "application/json" ] FileLoaded )

        ( _, FileLoaded file ) ->
            ( model, Task.perform ReadFile (File.toString file) )

        ( _, ReadFile content ) ->
            case Decode.decodeString Character.decoder content of
                Err err ->
                    ( DecodeErr (toNavKey model) err, Cmd.none )

                Ok character ->
                    ( Character (toNavKey model) character, Character.save character )

        ( _, LinkClicked urlRequest ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl (toNavKey model) (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( _, UrlChanged url ) ->
            changeRoute (Route.parse url) model

        ( _, _ ) ->
            ( model, Cmd.none )



-- INIT


init : Maybe String -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags _ navKey =
    ( case flags of
        Nothing ->
            Landing navKey

        Just json ->
            Character navKey (Character.decodeLocalCharacter json)
    , Nav.replaceUrl navKey (Route.toString Route.Root)
    )



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MAIN


main : Program (Maybe String) Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
