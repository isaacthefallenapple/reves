module Boon exposing (Assignment, Boon(..), assignments, decoder, encode, toString)

import Boon.Domain as Domain exposing (Domain(..))
import Boon.Resistance as Resistance exposing (Resistance(..))
import Boon.Skill as Skill exposing (Skill(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



-- import TypedDict


type Boon
    = GainResistance Resistance Int
    | GainDomains (List Domain)
    | GainSkills (List Skill)
    | GainEquipment (List String)
    | GainRefresh (List String)
    | GainSkillKnack Skill
    | GainDomainKnack Domain


toString : Boon -> String
toString boon =
    (case boon of
        GainResistance resistance bonus ->
            Resistance.toString resistance ++ "+" ++ String.fromInt bonus

        GainDomains domains ->
            let
                isMultiple =
                    List.length domains > 1
            in
            "Gain the "
                ++ String.join ", " (List.map Domain.toString domains)
                ++ " domain"
                ++ (if isMultiple then
                        "s"

                    else
                        ""
                   )

        GainSkills skills ->
            let
                isMultiple =
                    List.length skills > 1
            in
            "Gain the "
                ++ String.join ", " (List.map Skill.toString skills)
                ++ " skill"
                ++ (if isMultiple then
                        "s"

                    else
                        ""
                   )

        GainEquipment equipment ->
            "Gain " ++ String.join ", " equipment

        GainRefresh refresh ->
            "Gain " ++ String.join ", " refresh ++ " as a refresh"

        GainSkillKnack skill ->
            "Gain a Knack in the " ++ Skill.toString skill ++ " skill"

        GainDomainKnack domain ->
            "Gain a Knack in the " ++ Domain.toString domain ++ " domain"
    )
        ++ "."



-- ASSIGNMENTS


type alias Assignment =
    { name : String
    , boons : List Boon
    }


assignments : List Assignment
assignments =
    [ labour
    , personalAssistant
    , dogsbody
    , security
    , spy
    , testSubject
    , pleasure
    , searchAndRescue
    , escapee
    ]


labour : Assignment
labour =
    { name = "Labour"
    , boons =
        [ GainResistance Body 2
        , GainSkills [ Steal ]
        ]
    }


personalAssistant : Assignment
personalAssistant =
    { name = "Personal Assistant"
    , boons =
        [ GainResistance Resources 2
        , GainSkills [ Investigate ]
        ]
    }


dogsbody : Assignment
dogsbody =
    { name = "Dogsbody"
    , boons =
        [ GainResistance Resolve 2
        , GainDomains [ LowSociety ]
        ]
    }


security : Assignment
security =
    { name = "Security"
    , boons =
        [ GainResistance Body 2
        , GainSkills [ Scrap ]
        ]
    }


spy : Assignment
spy =
    { name = "Spy"
    , boons =
        [ GainResistance Shadow 2
        , GainSkills [ Deceive ]
        ]
    }


testSubject : Assignment
testSubject =
    { name = "Test Subject"
    , boons =
        [ GainSkills [ Resist ]
        , GainDomains [ Science ]
        ]
    }


pleasure : Assignment
pleasure =
    { name = "Pleasure"
    , boons =
        [ GainResistance Resources 2
        , GainDomains [ HighSociety ]
        ]
    }


searchAndRescue : Assignment
searchAndRescue =
    { name = "Search and Rescue"
    , boons =
        [ GainResistance Resolve 2
        , GainSkills [ Resist ]
        ]
    }


escapee : Assignment
escapee =
    { name = "Escapee"
    , boons =
        [ GainResistance Shadow 2
        , GainSkills [ Skulk ]
        ]
    }



-- ENCODE


encode : Boon -> Encode.Value
encode boon =
    Encode.object <|
        case boon of
            GainResistance resistance bonus ->
                [ ( "resistance", Resistance.encodeResistance resistance ), ( "bonus", Encode.int bonus ) ]

            GainDomains domain ->
                [ ( "domain", Encode.list Domain.encodeDomain domain ) ]

            GainSkills skill ->
                [ ( "skill", Encode.list Skill.encodeSkill skill ) ]

            GainEquipment equipment ->
                [ ( "equipment", Encode.list Encode.string equipment ) ]

            GainRefresh refresh ->
                [ ( "refresh", Encode.list Encode.string refresh ) ]

            GainSkillKnack skill ->
                [ ( "knack", Encode.string <| Skill.toString skill ) ]

            GainDomainKnack domain ->
                [ ( "knack", Encode.string <| Domain.toString domain ) ]



-- DECODER


resistanceUpDecoder : Decoder Boon
resistanceUpDecoder =
    Decode.map2 GainResistance
        (Decode.field "resistance" Resistance.resistanceDecoder)
        (Decode.field "bonus" Decode.int)


newSkillDecoder : Decoder Boon
newSkillDecoder =
    Decode.map GainSkills
        (Decode.field "skill" (Decode.list Skill.skillDecoder))


newDomainDecoder : Decoder Boon
newDomainDecoder =
    Decode.map GainDomains
        (Decode.field "domain" (Decode.list Domain.domainDecoder))


newEquipmentDecoder : Decoder Boon
newEquipmentDecoder =
    Decode.map GainEquipment
        (Decode.field "equipment" (Decode.list Decode.string))


newRefreshDecoder : Decoder Boon
newRefreshDecoder =
    Decode.map GainRefresh
        (Decode.field "refresh" (Decode.list Decode.string))


decoder : Decoder Boon
decoder =
    Decode.oneOf
        [ resistanceUpDecoder
        , newSkillDecoder
        , newDomainDecoder
        , newEquipmentDecoder
        , newRefreshDecoder
        , Decode.oneOf
            [ Decode.map GainSkillKnack Skill.skillDecoder
            , Decode.map GainDomainKnack Domain.domainDecoder
            ]
        ]
