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
    = ResistanceUp Resistance Int
    | NewDomain Domain
    | NewSkill Skill
    | NewAbility Ability
    | NewEquipment String
    | NewRefresh String



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
        [ ResistanceUp Body 2
        , NewSkill Steal
        ]
    }


personalAssistant : Assignment
personalAssistant =
    { name = "Personal Assistant"
    , boons =
        [ ResistanceUp Resources 2
        , NewSkill Investigate
        ]
    }


dogsbody : Assignment
dogsbody =
    { name = "Dogsbody"
    , boons =
        [ ResistanceUp Resolve 2
        , NewDomain LowSociety
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
        [ ResistanceUp Resolve 2
        , ResistanceUp Reputation 2
        , NewRefresh "Help those who cannot help themselves."
        , NewSkill Patch
        , NewSkill Compel
        , NewDomain Science
        , NewDomain LowSociety
        , NewEquipment "The common red medic outfit bearing the official white medic seal of the Hegemony."
        , NewEquipment "A less than ideally stocked medkit."
        , NewAbility
            { name = "Medical Attention"
            , flavor = Just "You take some time to check on your allies and stitch them up if need be."
            , boons = []
            , text = "Once per session, help your allies heal, physically and mentally. All allies present may restore 3 stress from Body or Resolve."
            }
        , NewAbility
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
        [ ResistanceUp Shadow 3
        , ResistanceUp Body 1
        , NewRefresh ""
        , NewSkill Skulk
        , NewSkill Scrap
        , NewDomain Criminal
        , NewDomain HighSociety
        , NewEquipment "Light body armour (Armour 2)"
        , NewEquipment "Climbing gear and ropes"
        , NewAbility
            { name = "Surprise Infiltration"
            , flavor = Just "Nothing can keep you out."
            , boons = []
            , text = "Once per session, insert yourself into a situation where you are not currently present, so long as there’s some conceivable way you could get in there."
            }
        , NewAbility
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
            ResistanceUp resistance bonus ->
                [ ( "resistance", Resistance.encodeResistance resistance ), ( "bonus", Encode.int bonus ) ]

            NewDomain domain ->
                [ ( "domain", Domain.encodeDomain domain ) ]

            NewSkill skill ->
                [ ( "skill", Skill.encodeSkill skill ) ]

            NewEquipment equipment ->
                [ ( "equipment", Encode.string equipment ) ]

            NewRefresh refresh ->
                [ ( "refresh", Encode.string refresh ) ]

            NewAbility ability ->
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
    Decode.map2 ResistanceUp
        (Decode.field "resistance" Resistance.resistanceDecoder)
        (Decode.field "bonus" Decode.int)


newSkillDecoder : Decoder Boon
newSkillDecoder =
    Decode.map NewSkill
        (Decode.field "skill" Skill.skillDecoder)


newDomainDecoder : Decoder Boon
newDomainDecoder =
    Decode.map NewDomain
        (Decode.field "domain" Domain.domainDecoder)


newEquipmentDecoder : Decoder Boon
newEquipmentDecoder =
    Decode.map NewEquipment
        (Decode.field "equipment" Decode.string)


newRefreshDecoder : Decoder Boon
newRefreshDecoder =
    Decode.map NewRefresh
        (Decode.field "refresh" Decode.string)


newAbilityDecoder : Decoder Boon
newAbilityDecoder =
    Decode.map NewAbility
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
