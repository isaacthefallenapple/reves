module Boon.Resistance exposing (Resistance(..), Resistances, decoder, encode, encodeResistance, fromString, new, resistanceDecoder, toString, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import TypedDict exposing (TypedDict)


type alias Resistances =
    TypedDict Resistance Int


new : Resistances
new =
    TypedDict.fromListWithDefault 0
        [ Body
        , Resolve
        , Resources
        , Shadow
        , Reputation
        , Armor
        ]


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
                                        (Maybe.withDefault 0 (TypedDict.get r resistances))
                                    )
                                , onInput
                                    (\s ->
                                        toMsg
                                            (TypedDict.set r
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


encode : Resistances -> Encode.Value
encode =
    TypedDict.encode toString Encode.int


encodeResistance : Resistance -> Encode.Value
encodeResistance resistance =
    Encode.string (toString resistance)



-- DECODER


decoder : Decode.Decoder Resistances
decoder =
    TypedDict.decoder (fromString >> Maybe.withDefault Body) Decode.int


resistanceDecoder : Decode.Decoder Resistance
resistanceDecoder =
    Decode.string
        |> Decode.andThen
            (fromString
                >> Maybe.map Decode.succeed
                >> Maybe.withDefault (Decode.fail "error")
            )
