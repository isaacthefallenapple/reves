module Boon.Knack exposing (..)

import Boon.Domain as Domain
import Boon.Skill as Skill
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import TypeDict as Dict exposing (Dict)
import TypeDict.Json.Decode as Decode
import TypeDict.Json.Encode as Encode


type alias Knacks =
    Dict String Type String


type Type
    = Domain Domain.Domain
    | Skill Skill.Skill


toString : Type -> String
toString ty =
    case ty of
        Domain d ->
            Domain.toString d

        Skill s ->
            Skill.toString s


fromString : String -> Maybe Type
fromString s =
    case ( Skill.fromString s, Domain.fromString s ) of
        ( Just skill, _ ) ->
            Just (Skill skill)

        ( _, Just domain ) ->
            Just (Domain domain)

        _ ->
            Nothing


getDomain : Domain.Domain -> Knacks -> Maybe String
getDomain =
    Domain >> Dict.get


getSkill : Skill.Skill -> Knacks -> Maybe String
getSkill =
    Skill >> Dict.get



-- ENCODE


encodeType : Type -> Value
encodeType =
    toString >> JE.string


encode : Knacks -> Value
encode =
    Encode.string (toString >> JE.string) JE.string



-- DECODER


typeDecoder : Decoder Type
typeDecoder =
    JD.oneOf
        [ JD.map Skill Skill.skillDecoder
        , JD.map Domain Domain.domainDecoder
        ]


decoder : Decoder Knacks
decoder =
    Decode.decoder toString
        typeDecoder
        JD.string
