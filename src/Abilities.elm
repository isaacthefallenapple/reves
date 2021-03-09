module Abilities exposing (..)

import Ability exposing (Ability)
import Character
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)


url : String -> String
url name =
    "data/abilities/" ++ name ++ ".json"



-- MODEL


type alias Advances =
    { low : List Ability
    , medium : List Ability
    , high : List Ability
    }


type alias Abilities =
    { character : Character.Stats
    , title : String
    , advances : Status Advances
    }


type Status a
    = Loading
    | Loaded a
    | Failed Http.Error



-- INIT


init : (Result Http.Error Advances -> msg) -> Character.Stats -> ( Abilities, Cmd msg )
init toMsg character =
    ( { character = character, title = character.class, advances = Loading }
    , Http.get
        { url =
            url character.class
        , expect =
            Http.expectJson toMsg decoder
        }
    )



-- UPDATE


update : Result Http.Error Advances -> Abilities -> Abilities
update result abilities =
    case result of
        Ok advances ->
            { abilities | advances = Loaded advances }

        Err err ->
            { abilities | advances = Failed err }



-- VIEW


view : Abilities -> Html msg
view { advances, title } =
    div
        []
        (case advances of
            Loading ->
                [ h1
                    []
                    [ text title ]
                ]

            Failed _ ->
                [ h1
                    []
                    [ text "Error :(" ]
                ]

            Loaded { low, medium, high } ->
                [ h1
                    []
                    [ text title ]
                , h2
                    []
                    [ text "Low" ]
                , ul
                    []
                    (List.map Ability.view low)
                , h2
                    []
                    [ text "Medium" ]
                , ul
                    []
                    (List.map Ability.view medium)
                , h2
                    []
                    [ text "High" ]
                , ul
                    []
                    (List.map Ability.view high)
                ]
        )



-- DECODER


decoder : Decoder Advances
decoder =
    Decode.map3 Advances
        (Decode.field "low" (Decode.list Ability.decoder))
        (Decode.field "medium" (Decode.list Ability.decoder))
        (Decode.field "high" (Decode.list Ability.decoder))
