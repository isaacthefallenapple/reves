module Abilities exposing (..)

import Ability exposing (Ability)
import Browser.Navigation as Nav
import Character
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Route
import Session exposing (Session)
import Status exposing (HttpStatus)


location : String
location =
    "/reves/data/abilities/"


metadataLocation : String
metadataLocation =
    location ++ "metadata.json"



-- MODEL


type alias Advances =
    { low : Dict String Ability
    , medium : Dict String Ability
    , high : Dict String Ability
    }


dedup : Dict String a -> Advances -> Advances
dedup abilities advances =
    { advances
        | low = Dict.diff advances.low abilities
        , medium = Dict.diff advances.medium abilities
        , high = Dict.diff advances.high abilities
    }


type alias Metadata =
    Dict String String


type alias Abilities =
    { session : Session

    -- , character : Character.Stats
    -- , primary : String
    , selected : String
    , metadata : HttpStatus Metadata
    , tabs : Dict String (HttpStatus Advances)
    , chosen : Dict String Ability
    }


setSelected : Maybe String -> Abilities -> Abilities
setSelected selected abilities =
    { abilities
        | selected =
            case ( selected, Session.character abilities.session ) of
                ( Just s, _ ) ->
                    s

                ( Nothing, Just c ) ->
                    c.class

                ( Nothing, Nothing ) ->
                    ""
    }


navKey : Abilities -> Nav.Key
navKey { session } =
    Session.navKey session


toSession : Abilities -> Session
toSession =
    .session


updateSession : Session -> Abilities -> Abilities
updateSession session abilities =
    { abilities | session = session }



-- MSG


type
    Msg
    -- = ClickedTab String
    = GotMetadata (Result Http.Error Metadata)
    | GotAdvances String (Result Http.Error Advances)
    | ChoseAbility Ability
    | UnchoseAbility String
    | ApplyChosen



-- INIT


init : Session -> Maybe String -> ( Abilities, Cmd Msg )
init session maybeSelected =
    ( setSelected maybeSelected
        { session = session
        , selected = ""
        , metadata = Status.Loading
        , tabs = Dict.empty
        , chosen = Dict.empty
        }
    , Cmd.batch
        [ Http.get
            { url =
                metadataLocation
            , expect =
                Http.expectJson GotMetadata metadataDecoder
            }
        ]
    )



-- UPDATE


update : Msg -> Abilities -> ( Abilities, Cmd Msg )
update msg abilities =
    let
        maybeCharacter =
            Session.character abilities.session
    in
    case msg of
        ChoseAbility ability ->
            let
                _ =
                    Debug.log "Chose ability" ()
            in
            ( { abilities | chosen = Dict.insert ability.name ability abilities.chosen }
            , Cmd.none
            )

        UnchoseAbility name ->
            ( { abilities | chosen = Dict.remove name abilities.chosen }, Cmd.none )

        GotMetadata (Ok metadata) ->
            ( { abilities
                | metadata = Status.Loaded metadata
                , tabs = Dict.map (always (always Status.Loading)) metadata
              }
            , fetchFromList metadata
            )

        GotMetadata (Err err) ->
            ( { abilities | metadata = Status.Failed err }, Cmd.none )

        GotAdvances name advances ->
            ( { abilities
                | tabs =
                    Dict.insert name
                        (advances
                            |> Status.fromResult
                            |> Status.map
                                (maybeCharacter
                                    |> Maybe.map .abilities
                                    |> Maybe.map dedup
                                    |> Maybe.withDefault identity
                                )
                        )
                        abilities.tabs
              }
            , Cmd.none
            )

        -- ClickedTab selected ->
        --     ( { abilities | selected = selected }
        --     , Nav.pushUrl (Session.navKey abilities.session) (Route.toString (Route.Abilities (Just selected)))
        --     )
        ApplyChosen ->
            -- let
            --     updatedCharacter =
            --         Character.addAbilities (Dict.values abilities.chosen) abilities.character
            -- in
            case Maybe.map (Character.addAbilities (Dict.values abilities.chosen)) maybeCharacter of
                Nothing ->
                    ( abilities, Cmd.none )

                Just updatedCharacter ->
                    let
                        session =
                            Session.setCharacter updatedCharacter abilities.session
                    in
                    Tuple.mapFirst
                        (\s ->
                            { abilities
                                | session = s
                                , tabs = Dict.map (\_ -> Status.map (dedup updatedCharacter.abilities)) abilities.tabs
                            }
                        )
                        (Session.save session)


fetchFromList : Metadata -> Cmd Msg
fetchFromList metadata =
    Cmd.batch
        (List.map
            (\( name, loc ) ->
                Http.get
                    { url = location ++ loc
                    , expect = Http.expectJson (GotAdvances name) decoder
                    }
            )
            (Dict.toList metadata)
        )



-- VIEW


view : Abilities -> Html Msg
view { session, selected, tabs, chosen } =
    let
        primary =
            Session.character session
                |> Maybe.map .class
                |> Maybe.withDefault ""

        primaryFirst =
            Dict.get primary tabs
                |> Maybe.map
                    (\advances ->
                        ( primary, advances )
                            :: Dict.toList (Dict.remove primary tabs)
                    )
                |> Maybe.withDefault (Dict.toList tabs)
    in
    div
        [ class "abilities-page"
        , class "wrapper"
        ]
        [ nav
            [ class "tab-bar" ]
            [ a
                [ href (Route.toString Route.Root) ]
                [ text "< Back" ]
            , ul
                [ attribute "role" "list"
                ]
                (List.map
                    (\( name, _ ) ->
                        li
                            [ classList [ ( "selected", name == selected ) ] ]
                            [ a
                                [ href (Route.toString (Route.Abilities (Just name))) ]
                                [ text name ]
                            ]
                    )
                    primaryFirst
                )
            ]
        , div
            [ class "abilities-page__advances" ]
            [ div
                []
              <|
                List.map
                    (\( name, advances ) ->
                        viewAdvances name chosen advances (name == selected)
                    )
                    primaryFirst
            , button
                [ class "button"
                , onClick ApplyChosen
                ]
                [ text "Apply" ]
            ]
        ]


viewAdvances : String -> Dict String Ability -> HttpStatus Advances -> Bool -> Html Msg
viewAdvances name selectedAbilities advances isSelected =
    div
        [ classList [ ( "hidden", not isSelected ) ] ]
        (case advances of
            Status.Loading ->
                [ h1
                    []
                    [ text ("Loading " ++ name) ]
                ]

            Status.Failed _ ->
                [ h1
                    []
                    [ text "Error :(" ]
                ]

            Status.Loaded { low, medium, high } ->
                [ h2
                    []
                    [ text "Low" ]
                , viewAdvanceList selectedAbilities low
                , h2
                    []
                    [ text "Medium" ]
                , viewAdvanceList selectedAbilities medium
                , h2
                    []
                    [ text "High" ]
                , viewAdvanceList selectedAbilities high
                ]
        )


viewAdvanceList : Dict String Ability -> Dict String Ability -> Html Msg
viewAdvanceList selectedAbilities abilities =
    ul
        [ attribute "role" "list"
        , class "abilities-page__list"
        , class "flow"
        ]
        (List.map
            (\ability ->
                let
                    isChosen =
                        Dict.member ability.name selectedAbilities
                in
                li
                    [ classList [ ( "chosen", isChosen ) ]
                    , onClick
                        (if isChosen then
                            UnchoseAbility ability.name

                         else
                            ChoseAbility ability
                        )
                    ]
                    [ Ability.view ability ]
            )
         <|
            Dict.values abilities
        )



-- DECODER


decoder : Decoder Advances
decoder =
    let
        listToDict =
            List.foldl (\ability -> Dict.insert ability.name ability) Dict.empty
    in
    Decode.map3 Advances
        (Decode.map listToDict (Decode.field "low" (Decode.list Ability.decoder)))
        (Decode.map listToDict (Decode.field "medium" (Decode.list Ability.decoder)))
        (Decode.map listToDict (Decode.field "high" (Decode.list Ability.decoder)))


metadataDecoder : Decoder Metadata
metadataDecoder =
    Decode.dict Decode.string
