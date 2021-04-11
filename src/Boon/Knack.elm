module Boon.Knack exposing (..)

import Array exposing (Array)
import Boon.Domain as Domain
import Boon.Skill as Skill
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import TypeDict as Dict exposing (Dict)
import TypeDict.Json.Decode as Decode
import TypeDict.Json.Encode as Encode


type alias Knacks k =
    Dict String k (Array String)


newSkills : Knacks Skill.Skill
newSkills =
    Dict.empty Skill.toString


newDomains : Knacks Domain.Domain
newDomains =
    Dict.empty Domain.toString


insert : k -> String -> Knacks k -> Knacks k
insert key val =
    Dict.update key
        (\v ->
            case v of
                Nothing ->
                    Just <| Array.fromList [ val ]

                Just ks ->
                    Just <| Array.push val ks
        )


update : k -> Int -> String -> Knacks k -> Knacks k
update key idx val =
    if String.isEmpty val then
        remove key idx

    else
        Dict.update key
            (Maybe.map <| Array.set idx val)


remove : k -> Int -> Knacks k -> Knacks k
remove key idx =
    Dict.update key
        (Maybe.map
            (\knacks ->
                let
                    start =
                        Array.slice 0 idx knacks

                    end =
                        Array.slice (idx + 1) (Array.length knacks) knacks
                in
                Array.append start end
            )
        )



-- ENCODE


encode : Knacks k -> Value
encode knacks =
    Encode.string (Dict.toStringer knacks >> JE.string) (JE.array JE.string) knacks



-- DECODER


skillsDecoder : Decoder (Knacks Skill.Skill)
skillsDecoder =
    Decode.decoder Skill.toString
        Skill.skillDecoder
        (JD.array JD.string)


domainsDecoder : Decoder (Knacks Domain.Domain)
domainsDecoder =
    Decode.decoder Domain.toString
        Domain.domainDecoder
        (JD.array JD.string)
