module Boon.Domain exposing (..)

import Debug exposing (toString)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import TypeDict exposing (Dict)
import TypeDict.Json.Decode as Decode
import TypeDict.Json.Encode as Encode


type Domain
    = Criminal
    | HighSociety
    | LowSociety
    | Weirdness
    | Hegemony
    | Science


type alias Domains =
    Dict String Domain Bool


new : Domains
new =
    TypeDict.fromList toString
        (List.map (\d -> ( d, False ))
            [ Criminal
            , HighSociety
            , LowSociety
            , Weirdness
            , Hegemony
            , Science
            ]
        )


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


decoder : Decoder Domains
decoder =
    Decode.decoder toString domainDecoder JD.bool


domainDecoder : Decoder Domain
domainDecoder =
    JD.string
        |> JD.andThen
            (fromString
                >> Maybe.map JD.succeed
                >> Maybe.withDefault (JD.fail "error")
            )



-- ENCODE


encodeDomain : Domain -> Value
encodeDomain domain =
    JE.string (toString domain)


encode : Domains -> Value
encode =
    Encode.string encodeDomain JE.bool
