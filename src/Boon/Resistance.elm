module Boon.Resistance exposing (Resistance(..), Resistances, decoder, encode, encodeResistance, fromString, new, resistanceDecoder, toString, view)

-- import Dict

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import TypeDict as Dict exposing (Dict)
import TypeDict.Json.Decode as Decode
import TypeDict.Json.Encode as Encode


type alias Resistances =
    Dict String Resistance Int


new : Resistances
new =
    Dict.fromList
        toString
        (List.map (\r -> ( r, 0 ))
            [ Body
            , Resolve
            , Resources
            , Shadow
            , Reputation
            , Armor
            ]
        )


type Resistance
    = Body
    | Resolve
    | Resources
    | Shadow
    | Reputation
    | Armor


toString : Resistance -> String
toString resistance =
    case resistance of
        Body ->
            "Body"

        Resolve ->
            "Resolve"

        Resources ->
            "Resources"

        Shadow ->
            "Shadow"

        Reputation ->
            "Reputation"

        Armor ->
            "Armour"


fromString : String -> Maybe Resistance
fromString s =
    case s of
        "Body" ->
            Just Body

        "Resolve" ->
            Just Resolve

        "Resources" ->
            Just Resources

        "Shadow" ->
            Just Shadow

        "Reputation" ->
            Just Reputation

        "Armour" ->
            Just Armor

        _ ->
            Nothing



-- VIEW


view : (Resistances -> msg) -> Resistances -> Html msg
view toMsg resistances =
    let
        resistancesList =
            [ Body, Resolve, Resources, Shadow, Reputation, Armor ]
    in
    table
        []
        [ thead
            []
            [ tr
                []
                (List.map (toString >> text >> List.singleton >> th [])
                    resistancesList
                )
            ]
        , tbody
            []
            [ tr
                []
                (List.map
                    (\r ->
                        td
                            []
                            [ input
                                [ type_ "number"
                                , value
                                    (String.fromInt
                                        (Maybe.withDefault 0 (Dict.get r resistances))
                                    )
                                , onInput
                                    (\s ->
                                        toMsg
                                            (Dict.insert r
                                                (s
                                                    |> String.toInt
                                                    |> Maybe.withDefault 0
                                                    |> clamp 0 5
                                                )
                                                resistances
                                            )
                                    )
                                ]
                                []
                            ]
                    )
                    resistancesList
                )
            ]
        ]



-- ENCODE


encode : Resistances -> JE.Value
encode =
    Encode.string encodeResistance JE.int


encodeResistance : Resistance -> Value
encodeResistance resistance =
    JE.string (toString resistance)



-- DECODER


decoder : Decoder Resistances
decoder =
    Decode.decoder toString resistanceDecoder JD.int


resistanceDecoder : Decoder Resistance
resistanceDecoder =
    JD.string
        |> JD.andThen
            (fromString
                >> Maybe.map JD.succeed
                >> Maybe.withDefault (JD.fail "error")
            )
