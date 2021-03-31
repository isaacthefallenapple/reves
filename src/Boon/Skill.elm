module Boon.Skill exposing (..)

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import TypeDict as Dict exposing (Dict)
import TypeDict.Json.Decode as Decode
import TypeDict.Json.Encode as Encode


type Skill
    = Compel
    | Deceive
    | Hack
    | Investigate
    | Patch
    | Resist
    | Scramble
    | Scrap
    | Skulk
    | Steal


type alias Skills =
    Dict String Skill Bool


new : Skills
new =
    Dict.fromList toString
        (List.map
            (\s -> ( s, False ))
            [ Compel
            , Deceive
            , Hack
            , Patch
            , Scramble
            , Scrap
            , Skulk
            , Investigate
            , Steal
            , Resist
            ]
        )


toString : Skill -> String
toString skill =
    case skill of
        Compel ->
            "Compel"

        Deceive ->
            "Deceive"

        Hack ->
            "Hack"

        Patch ->
            "Patch"

        Scramble ->
            "Scramble"

        Scrap ->
            "Scrap"

        Skulk ->
            "Skulk"

        Investigate ->
            "Investigate"

        Steal ->
            "Steal"

        Resist ->
            "Resist"


fromString : String -> Maybe Skill
fromString s =
    case s of
        "Compel" ->
            Just Compel

        "Deceive" ->
            Just Deceive

        "Hack" ->
            Just Hack

        "Patch" ->
            Just Patch

        "Scramble" ->
            Just Scramble

        "Scrap" ->
            Just Scrap

        "Skulk" ->
            Just Skulk

        "Investigate" ->
            Just Investigate

        "Steal" ->
            Just Steal

        "Resist" ->
            Just Resist

        _ ->
            Nothing



-- ENCODE


encode : Skills -> Value
encode =
    Encode.string encodeSkill JE.bool


encodeSkill : Skill -> Value
encodeSkill skill =
    JE.string (toString skill)



-- DECODER


decoder : Decoder Skills
decoder =
    Decode.decoder toString skillDecoder JD.bool


skillDecoder : Decoder Skill
skillDecoder =
    JD.string
        |> JD.andThen
            (fromString
                >> Maybe.map JD.succeed
                >> Maybe.withDefault (JD.fail "error")
            )
