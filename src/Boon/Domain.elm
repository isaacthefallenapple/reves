module Boon.Domain exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import TypedDict exposing (TypedDict)


type Domain
    = Criminal
    | HighSociety
    | LowSociety
    | Weirdness
    | Hegemony
    | Science


type alias Domains =
    TypedDict Domain Bool


new : Domains
new =
    TypedDict.fromListWithDefault False
        [ Criminal
        , HighSociety
        , LowSociety
        , Weirdness
        , Hegemony
        , Science
        ]


toString : Domain -> String
toString domain =
    case domain of
        Criminal ->
            "Criminal"

        HighSociety ->
            "High Society"

        LowSociety ->
            "Low Society"

        Weirdness ->
            "Weirdness"

        Hegemony ->
            "Hegemony"

        Science ->
            "Science"


fromString : String -> Maybe Domain
fromString s =
    case s of
        "Criminal" ->
            Just Criminal

        "High Society" ->
            Just HighSociety

        "Low Society" ->
            Just LowSociety

        "Weirdness" ->
            Just Weirdness

        "Hegemony" ->
            Just Hegemony

        "Science" ->
            Just Science

        _ ->
            Nothing



-- DECODER


decoder : Decode.Decoder Domains
decoder =
    TypedDict.decoder (fromString >> Maybe.withDefault LowSociety) Decode.bool


domainDecoder : Decode.Decoder Domain
domainDecoder =
    Decode.string
        |> Decode.andThen
            (fromString
                >> Maybe.map Decode.succeed
                >> Maybe.withDefault (Decode.fail "error")
            )



-- ENCODE


encodeDomain : Domain -> Encode.Value
encodeDomain domain =
    Encode.string (toString domain)


encode : Domains -> Encode.Value
encode =
    TypedDict.encode toString Encode.bool
