module Boon.Skill exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import TypedDict exposing (TypedDict)


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
    TypedDict Skill Bool


new : Skills
new =
    TypedDict.fromListWithDefault False
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


encode : Skills -> Encode.Value
encode =
    TypedDict.encode toString Encode.bool


encodeSkill : Skill -> Encode.Value
encodeSkill skill =
    Encode.string (toString skill)



-- DECODER


decoder : Decode.Decoder Skills
decoder =
    TypedDict.decoder (fromString >> Maybe.withDefault Compel) Decode.bool


skillDecoder : Decode.Decoder Skill
skillDecoder =
    Decode.string
        |> Decode.andThen
            (fromString
                >> Maybe.map Decode.succeed
                >> Maybe.withDefault (Decode.fail "error")
            )
