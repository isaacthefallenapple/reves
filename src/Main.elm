module Main exposing (main)

import Abilities exposing (Abilities)
import Boon
import Browser
import Browser.Navigation as Nav
import Character exposing (Msg(..))
import Class
import Dict exposing (Dict)
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import PlayAids
import Ports
import Route exposing (Route)
import Session exposing (Session)
import Task
import Url



-- TYPES


type Model
    = PickClass Session
    | PickAssignment Session Class.Class
    | Character Session
    | DecodeErr Session Decode.Error
    | Abilities Abilities
    | Landing Session
    | PlayAid Session PlayAids.PlayAids


toSession : Model -> Session
toSession model =
    case model of
        PickClass session ->
            session

        PickAssignment session _ ->
            session

        Character session ->
            session

        DecodeErr session _ ->
            session

        Abilities abilities ->
            Abilities.toSession abilities

        Landing session ->
            session

        PlayAid session _ ->
            session


updateSession : Session -> Model -> Model
updateSession session model =
    case model of
        PickClass _ ->
            PickClass session

        PickAssignment _ val ->
            PickAssignment session val

        Character _ ->
            Character session

        DecodeErr _ err ->
            DecodeErr session err

        Abilities abilities ->
            Abilities (Abilities.updateSession session abilities)

        Landing _ ->
            Landing session

        PlayAid _ aid ->
            PlayAid session aid



-- MSG


type Msg
    = PickedClass Class.Class
    | PickedAssignment Boon.Assignment
    | CharacterMsg Character.Msg
    | ClickedOpenFile
    | FileLoaded File
    | ReadFile String
    | RequestedAbilities (Result Http.Error Abilities.Advances)
    | ClickedSave
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | AbilitiesMsg Abilities.Msg
    | ClickedNewCharacter
    | GotPlayAid (Result Http.Error (Dict String String))
    | SavedChanges



-- ENCODE
-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model of
        DecodeErr _ err ->
            { title = "Error"
            , body =
                [ div
                    [ class "wrapper gap-top-700 flow" ]
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
                ]
            }

        Landing _ ->
            { title = "Welcome!"
            , body =
                [ div
                    [ class "wrapper gap-top-700 flow" ]
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
                ]
            }

        PickClass _ ->
            { title = "Pick a class"
            , body =
                [ div
                    [ class "class-picker"
                    , class "wrapper gap-top-700 flow"
                    ]
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
                    [ class "assignment-picker"
                    , class "wrapper gap-top-700 flow"
                    ]
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

        Character session ->
            case Session.character session of
                Just character ->
                    let
                        characterView =
                            Character.view character
                    in
                    { title = characterView.title
                    , body =
                        List.map (Html.map CharacterMsg) characterView.body
                            ++ [ footer
                                    [ class "footer"
                                    , class "bg-light-shade"
                                    ]
                                    [ span
                                        []
                                        [ text (Session.changesToString (Session.changes session))
                                        ]
                                    , button
                                        [ onClick ClickedSave
                                        , class "button"
                                        , classList [ ( "unsaved", not (Session.isSaved session) ) ]
                                        ]
                                        [ text "Save" ]
                                    ]
                               ]
                    }

                Nothing ->
                    { title = "New Character"
                    , body =
                        [ p
                            []
                            [ text "Looks like you don't have a character yet. Wanna "
                            , a
                                [ href "/reves/" ]
                                [ text "make one" ]
                            , text "?"
                            ]
                        ]
                    }

        Abilities abilities ->
            { title = abilities.selected
            , body = [ Html.map AbilitiesMsg (Abilities.view abilities) ]
            }

        PlayAid _ playAid ->
            PlayAids.view playAid



-- UPDATE


wrap : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
wrap toModel toMsg ( subModel, subMsg ) =
    ( toModel subModel, Cmd.map toMsg subMsg )


changeRoute : Route -> Model -> ( Model, Cmd Msg )
changeRoute route model =
    let
        navKey =
            toSession model
    in
    case ( model, route ) of
        ( Character session, Route.Root ) ->
            case Session.character session of
                Just _ ->
                    ( model, Cmd.none )

                Nothing ->
                    ( Landing session, Cmd.none )

        ( Abilities abilities, Route.Root ) ->
            ( Character (Abilities.toSession abilities), Cmd.none )

        ( Abilities abilities, Route.Abilities selected ) ->
            ( Abilities (Abilities.setSelected selected abilities), Cmd.none )

        ( Character session, Route.Abilities selected ) ->
            wrap Abilities AbilitiesMsg (Abilities.init session selected)

        ( PickClass session, Route.Root ) ->
            ( Landing session, Cmd.none )

        ( PickAssignment _ _, Route.Root ) ->
            ( Landing navKey, Cmd.none )

        ( Character session, Route.PlayAid topic selected ) ->
            wrap (PlayAid session) identity (PlayAids.init GotPlayAid topic selected)

        ( PlayAid session _, Route.Root ) ->
            ( Character session, Cmd.none )

        ( PlayAid session _, Route.PlayAid topic selected ) ->
            wrap (PlayAid session) identity (PlayAids.init GotPlayAid topic selected)

        ( _, _ ) ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Landing session, ClickedNewCharacter ) ->
            ( PickClass session, Cmd.none )

        ( PickClass session, PickedClass class ) ->
            ( PickAssignment session class, Cmd.none )

        ( PickAssignment session class, PickedAssignment assignment ) ->
            let
                character =
                    Character.blank
                        |> Character.applyClass class
                        |> Character.applyAssignment assignment

                updatedSession =
                    Session.setCharacter character session
            in
            Tuple.mapFirst Character
                (Session.save updatedSession)

        ( Character session, ClickedSave ) ->
            let
                ( updatedSession, cmd ) =
                    Session.save (Session.savedChanges session)
            in
            ( Character updatedSession
            , case Session.character session of
                Just character ->
                    Cmd.batch
                        [ Download.string character.name
                            "application/json"
                            (Encode.encode 2 (Character.encode character))
                        , Ports.savedCharacter ()
                        , cmd
                        ]

                Nothing ->
                    Cmd.none
            )

        ( Character session, CharacterMsg subMsg ) ->
            case Session.character session of
                Just character ->
                    let
                        ( updatedSession, cmd ) =
                            Session.save <| Session.setCharacter (Character.update subMsg character) session
                    in
                    ( Character updatedSession
                    , Cmd.batch
                        [ Ports.updatedCharacter ()
                        , cmd
                        ]
                    )

                Nothing ->
                    ( model, Cmd.none )

        ( Abilities abilities, AbilitiesMsg subMsg ) ->
            Tuple.mapBoth Abilities (Cmd.map AbilitiesMsg) (Abilities.update subMsg abilities)

        ( PlayAid session playAid, GotPlayAid result ) ->
            ( PlayAid session (PlayAids.update result playAid), Cmd.none )

        ( _, ClickedOpenFile ) ->
            ( model, Select.file [ "application/json" ] FileLoaded )

        ( _, FileLoaded file ) ->
            ( model, Task.perform ReadFile (File.toString file) )

        ( _, ReadFile content ) ->
            case Decode.decodeString Character.decoder content of
                Err err ->
                    ( DecodeErr (toSession model) err, Cmd.none )

                Ok character ->
                    let
                        ( session, cmd ) =
                            Session.save <| Session.fromDisk character (toSession model)
                    in
                    ( Character session, cmd )

        ( _, LinkClicked urlRequest ) ->
            case urlRequest of
                Browser.Internal url ->
                    let
                        route =
                            Route.parse url

                        navKey =
                            Session.navKey (toSession model)

                        urlString =
                            Url.toString url
                    in
                    case ( model, route ) of
                        ( Abilities _, Route.Abilities _ ) ->
                            ( model, Nav.replaceUrl navKey urlString )

                        ( PlayAid _ _, Route.PlayAid _ _ ) ->
                            ( model, Nav.replaceUrl navKey urlString )

                        ( _, _ ) ->
                            ( model, Nav.pushUrl navKey urlString )

                Browser.External href ->
                    ( model, Nav.load href )

        ( _, UrlChanged url ) ->
            changeRoute (Debug.log "UrlChanged" (Route.parse url)) model

        ( _, SavedChanges ) ->
            ( updateSession
                (Session.savedChangesLocally (toSession model))
                model
            , Cmd.none
            )

        ( _, _ ) ->
            ( model, Cmd.none )



-- INIT


init : Maybe String -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        model =
            case flags of
                Nothing ->
                    Landing (Session.new navKey)

                Just json ->
                    Decode.decodeString (Session.decoder navKey) json
                        |> Result.withDefault (Session.new navKey)
                        |> Character

        cmd =
            if Session.isSaved (toSession model) then
                Cmd.none

            else
                Ports.updatedCharacter ()
    in
    Tuple.mapSecond (List.singleton >> (::) cmd >> Cmd.batch) <|
        changeRoute
            (Route.parse url)
            model



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.confirmLocalStorage (\_ -> SavedChanges)



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
