module PlayAids exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode
import Status exposing (HttpStatus)
import Url.Builder as UrlBuilder



-- import Html.Events exposing (..)


location : String
location =
    "/reves/data/play-aids/"


type alias PlayAids =
    { topic : Topic
    , selected : Maybe String
    , aids : HttpStatus (Dict String String)
    }



-- TOPIC


type Topic
    = Weapons
    | Armor
    | Skills
    | Domains


topicFromString : String -> Maybe Topic
topicFromString topic =
    case topic of
        "weapons" ->
            Just Weapons

        "armour" ->
            Just Armor

        "skills" ->
            Just Skills

        "domains" ->
            Just Domains

        _ ->
            Nothing


topicToString : Topic -> String
topicToString topic =
    case topic of
        Weapons ->
            "weapons"

        Armor ->
            "armour"

        Skills ->
            "skills"

        Domains ->
            "domains"


topicToStringPretty : Topic -> String
topicToStringPretty topic =
    let
        stringified =
            topicToString topic
    in
    String.toUpper (String.left 1 stringified) ++ String.dropLeft 1 stringified



-- VIEW


view : PlayAids -> Browser.Document msg
view playAid =
    let
        stringifiedTopic =
            topicToStringPretty playAid.topic
    in
    { title = stringifiedTopic
    , body =
        [ div
            [ class "wrapper" ]
            [ nav
                [ class "tab-bar" ]
                [ a
                    [ href "/reves/" ]
                    [ text "< Back" ]
                , ul
                    [ attribute "role" "list"

                    -- , class "tab-bar__list"
                    ]
                  <|
                    List.map
                        (\topic ->
                            li
                                [ classList [ ( "selected", topic == playAid.topic ) ] ]
                                [ a
                                    [ href <| UrlBuilder.absolute [ "reves", "play-aid", topicToString topic ] []
                                    ]
                                    [ text (topicToStringPretty topic) ]
                                ]
                        )
                        [ Weapons, Armor, Skills, Domains ]
                ]
            , article
                [ class "play-aid"
                ]
                [ h1
                    []
                    [ text stringifiedTopic ]
                , case playAid.aids of
                    Status.Failed _ ->
                        text "Error :("

                    Status.Loading ->
                        text "Loading..."

                    Status.Loaded aids ->
                        dl
                            []
                            (Dict.toList aids
                                |> List.map
                                    (\( name, rules ) ->
                                        [ dt
                                            [ id name
                                            ]
                                            [ b
                                                []
                                                [ text name ]
                                            ]
                                        , dd
                                            []
                                            [ text rules ]
                                        ]
                                    )
                                |> List.concat
                            )
                ]
            ]
        ]
    }



-- INIT


init : (Result Http.Error (Dict String String) -> msg) -> Topic -> Maybe String -> ( PlayAids, Cmd msg )
init toMsg topic maybeSelected =
    ( { topic = topic
      , selected = maybeSelected
      , aids = Status.Loading
      }
    , Http.get
        { url = location ++ topicToString topic ++ ".json"
        , expect = Http.expectJson toMsg (Decode.dict Decode.string)
        }
    )



-- UPDATE


update : Result Http.Error (Dict String String) -> PlayAids -> PlayAids
update result playAids =
    { playAids | aids = Status.fromResult result }
