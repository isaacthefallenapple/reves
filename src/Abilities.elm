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


location : String
location =
    "/data/abilities/"


metadataLocation : String
metadataLocation =
    location ++ "metadata.json"



-- MODEL


type alias Advances =
    { low : List Ability
    , medium : List Ability
    , high : List Ability
    }


dedup : Dict String a -> Advances -> Advances
dedup abilities advances =
    let
        innerDedup =
            List.filter (\ability -> not (Dict.member ability.name abilities))
    in
    { advances
        | low = innerDedup advances.low
        , medium = innerDedup advances.medium
        , high = innerDedup advances.high
    }


type alias Metadata =
    Dict String String


type alias Abilities =
    { navKey : Nav.Key
    , character : Character.Stats
    , primary : String
    , selected : String
    , metadata : Status Metadata
    , tabs : Dict String (Status Advances)
    , chosen : Dict String Ability
    }


toNavKey : Abilities -> Nav.Key
toNavKey =
    .navKey


type Status a
    = Loading
    | Loaded a
    | Failed Http.Error


statusFromResult : Result Http.Error a -> Status a
statusFromResult result =
    case result of
        Ok ok ->
            Loaded ok

        Err err ->
            Failed err



-- MSG


type Msg
    = ClickedTab String
    | GotMetadata (Result Http.Error Metadata)
    | GotAdvances String (Result Http.Error Advances)
    | ChoseAbility Ability
    | UnchoseAbility String
    | ApplyChosen



-- INIT


init : Nav.Key -> Maybe String -> Character.Stats -> ( Abilities, Cmd Msg )
init navKey maybeSelected character =
    ( { navKey = navKey
      , character = character
      , primary = character.class
      , selected = Maybe.withDefault character.class maybeSelected
      , metadata = Loading
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
                | metadata = Loaded metadata
                , tabs = Dict.map (always (always Loading)) metadata
              }
            , fetchFromList metadata
            )

        GotMetadata (Err err) ->
            ( { abilities | metadata = Failed err }, Cmd.none )

        GotAdvances name advances ->
            ( { abilities
                | tabs =
                    Dict.insert name
                        (statusFromResult
                            (Result.map (dedup abilities.character.abilities) advances)
                        )
                        abilities.tabs
              }
            , Cmd.none
            )

        ClickedTab selected ->
            ( { abilities | selected = selected }
            , Nav.pushUrl abilities.navKey (Route.toString (Route.Abilities (Just selected)))
            )

        ApplyChosen ->
            let
                updatedCharacter =
                    Character.addAbilities (Dict.values abilities.chosen) abilities.character
            in
            ( { abilities | character = updatedCharacter }, Character.save updatedCharacter )


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
view abilities =
    let
        primary =
            abilities.primary

        selected =
            abilities.selected

        tabs =
            abilities.tabs

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
        [ class "wrapper" ]
        (nav
            []
            [ a
                [ href "/" ]
                [ text "Back" ]
            , ul
                [ class "tab-bar" ]
                (List.map
                    (\( name, _ ) ->
                        li
                            [ classList [ ( "selected", name == selected ), ( "tab", True ) ] ]
                            [ button
                                [ onClick (ClickedTab name) ]
                                [ text name ]
                            ]
                    )
                    primaryFirst
                )
            ]
            :: List.map
                (\( name, advances ) ->
                    viewAdvances name abilities.chosen advances (name == selected)
                )
                primaryFirst
        )


viewAdvances : String -> Dict String Ability -> Status Advances -> Bool -> Html Msg
viewAdvances name selectedAbilities advances isSelected =
    div
        [ classList [ ( "hidden", not isSelected ) ] ]
        (case advances of
            Loading ->
                [ h1
                    []
                    [ text ("Loading " ++ name) ]
                ]

            Failed _ ->
                [ h1
                    []
                    [ text "Error :(" ]
                ]

            Loaded { low, medium, high } ->
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
                , button
                    [ onClick ApplyChosen ]
                    [ text "Apply" ]
                ]
        )


viewAdvanceList : Dict String Ability -> List Ability -> Html Msg
viewAdvanceList selectedAbilities abilities =
    ul
        []
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
            abilities
        )



-- DECODER


decoder : Decoder Advances
decoder =
    Decode.map3 Advances
        (Decode.field "low" (Decode.list Ability.decoder))
        (Decode.field "medium" (Decode.list Ability.decoder))
        (Decode.field "high" (Decode.list Ability.decoder))


metadataDecoder : Decoder Metadata
metadataDecoder =
    Decode.dict Decode.string
