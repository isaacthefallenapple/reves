module Boon exposing (Ability, Assignment, Boon(..), Class, abilityDecoder, assignments, classes, cloak, doc, encodeAbility, viewAbility)

import Boon.Domain as Domain exposing (Domain(..))
import Boon.Resistance as Resistance exposing (Resistance(..))
import Boon.Skill as Skill exposing (Skill(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



-- import TypedDict


type alias Ability =
    { name : String
    , flavor : Maybe String
    , boons : List Boon
    , text : String
    }


type Boon
    = GainResistance Resistance Int
    | GainDomains (List Domain)
    | GainSkills (List Skill)
    | GainAbility Ability
    | GainEquipment (List String)
    | GainRefresh (List String)



-- ASSIGNMENTS


type alias Assignment =
    { name : String
    , boons : List Boon
    }


assignments : List Assignment
assignments =
    [ labour, personalAssistant, dogsbody ]


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



-- CLASSES


type alias Class =
    { name : String
    , boons : List Boon
    }


classes : List Class
classes =
    [ doc
    , cloak
    ]


doc : Class
doc =
    { name = "Doc"
    , boons =
        [ GainResistance Resolve 2
        , GainResistance Reputation 2
        , GainRefresh [ "Help those who cannot help themselves." ]
        , GainSkills [ Patch, Compel ]
        , GainDomains [ Science, LowSociety ]
        , GainEquipment [ "The common red medic outfit bearing the official white medic seal of the Hegemony.", "A less than ideally stocked medkit." ]
        , GainAbility
            { name = "Medical Attention"
            , flavor = Just "You take some time to check on your allies and stitch them up if need be."
            , boons = []
            , text = "Once per session, help your allies heal, physically and mentally. All allies present may restore 3 stress from Body or Resolve."
            }
        , GainAbility
            { name = "Bedside Manner"
            , flavor = Just "People are grateful for your help."
            , boons = []
            , text = "When you heal someone, gain a bond with them until the end of the next day."
            }
        ]
    }


cloak : Class
cloak =
    { name = "Cloak"
    , boons =
        [ GainResistance Shadow 3
        , GainResistance Body 1
        , GainRefresh [ "" ]
        , GainSkills [ Skulk, Scrap ]
        , GainDomains [ Criminal, HighSociety ]
        , GainEquipment [ "Light body armour (Armour 2)", "Climbing gear and ropes" ]
        , GainAbility
            { name = "Surprise Infiltration"
            , flavor = Just "Nothing can keep you out."
            , boons = []
            , text = "Once per session, insert yourself into a situation where you are not currently present, so long as there’s some conceivable way you could get in there."
            }
        , GainAbility
            { name = "Tactician"
            , flavor = Just "You are a trained infiltrator, and others would do well to heed your words."
            , boons = []
            , text = "When you enter a dangerous situation, you can name up to three features or opportunities that your allies can take advantage of. The first time you or an ally uses an opportunity, they roll with mastery (for example: cover with a good view of the battlefield, an exit, a badly-guarded door, a stack of barrels, etc.)."
            }
        ]
    }



-- VIEW


viewAbility : Ability -> Html msg
viewAbility ability =
    details
        [ class "ability" ]
        [ summary
            []
            [ h3 [] [ text ability.name ] ]
        , p
            []
            ((ability.flavor
                |> Maybe.map (\flavor -> [ i [] [ text (flavor ++ " ") ] ])
                |> Maybe.withDefault []
             )
                ++ [ text ability.text
                   ]
            )
        ]



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

            GainAbility ability ->
                [ ( "ability", encodeAbility ability ) ]


encodeAbility : Ability -> Encode.Value
encodeAbility ability =
    Encode.object
        [ ( "name", Encode.string ability.name )
        , ( "flavor"
          , ability.flavor
                |> Maybe.map Encode.string
                |> Maybe.withDefault Encode.null
          )
        , ( "boons", Encode.list encode ability.boons )
        , ( "text", Encode.string ability.text )
        ]



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


newAbilityDecoder : Decoder Boon
newAbilityDecoder =
    Decode.map GainAbility
        (Decode.field "ability" abilityDecoder)


decoder : Decoder Boon
decoder =
    Decode.oneOf
        [ resistanceUpDecoder
        , newSkillDecoder
        , newDomainDecoder
        , newEquipmentDecoder
        , newRefreshDecoder
        , newAbilityDecoder
        ]


abilityDecoder : Decode.Decoder Ability
abilityDecoder =
    Decode.map4 Ability
        (Decode.field "name" Decode.string)
        (Decode.field "flavor" (Decode.nullable Decode.string))
        (Decode.field "boons" (Decode.list (Decode.lazy (\_ -> decoder))))
        (Decode.field "text" Decode.string)
