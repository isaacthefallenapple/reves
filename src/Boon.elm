module Boon exposing (Ability, Boon(..), abilityDecoder, doc, encodeAbility, viewAbility)

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


type alias Class =
    List Boon


doc : Class
doc =
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
        { name = "Triage"
        , flavor = Just "You can sense who most needs your help."
        , boons = [ ResistanceUp Reputation 1 ]
        , text = "Once per situation, ask the GM which NPC is most in need of help, and they’ll tell you."
        }
    ]



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
