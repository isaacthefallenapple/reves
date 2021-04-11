module Character exposing (Msg(..), Stats, addAbilities, applyAssignment, applyBoons, applyClass, blank, decodeLocalCharacter, decoder, encode, save, update, view)

-- import Ability exposing (Ability)

import Ability exposing (Ability)
import Array exposing (Array)
import Boon exposing (Boon(..))
import Boon.Domain as Domains exposing (Domains)
import Boon.Knack as Knacks exposing (Knacks)
import Boon.Resistance as Resistances exposing (Resistances)
import Boon.Skill as Skills exposing (Skills)
import Browser
import Class exposing (Class)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import PlayAids
import Ports
import Route
import TypeDict


type alias Stats =
    { name : String
    , class : String
    , assignment : String
    , skills : Skills
    , domains : Domains
    , skillKnacks : Knacks Skills.Skill
    , domainKnacks : Knacks Domains.Domain
    , equipment : String
    , refresh : String
    , abilities : Dict String Ability
    , bonds : String
    , fallout : String
    , resistances : Resistances
    , notes : String
    }


blank : Stats
blank =
    { name = ""
    , class = ""
    , assignment = ""
    , skills = Skills.new
    , domains = Domains.new
    , skillKnacks = Knacks.newSkills
    , domainKnacks = Knacks.newDomains
    , equipment = ""
    , refresh = ""
    , abilities = Dict.empty
    , bonds = ""
    , fallout = ""
    , resistances = Resistances.new
    , notes = ""
    }


save : Stats -> Cmd msg
save character =
    Encode.encode 2 (encode character) |> Ports.storeCharacter


applyClass : Class -> Stats -> Stats
applyClass { name, boons, coreAbilities } character =
    addAbilities coreAbilities
        (applyBoons boons
            { character
                | class = name
            }
        )


applyAssignment : Boon.Assignment -> Stats -> Stats
applyAssignment { name, boons } character =
    applyBoons boons { character | assignment = name }


applyBoon : Boon -> Stats -> Stats
applyBoon boon character =
    case boon of
        GainResistance resistance bonus ->
            { character | resistances = TypeDict.update resistance (Maybe.map ((+) bonus >> clamp 0 5)) character.resistances }

        GainDomains domains ->
            { character | domains = List.foldl (\k doms -> TypeDict.insert k True doms) character.domains domains }

        GainSkills skills ->
            { character | skills = List.foldl (\k s -> TypeDict.insert k True s) character.skills skills }

        GainEquipment equipment ->
            { character | equipment = character.equipment ++ String.join "\n\n" equipment ++ "\n\n" }

        GainRefresh refresh ->
            { character | refresh = character.refresh ++ String.join "\n\n" refresh ++ "\n\n" }

        GainSkillKnack skill ->
            { character | skillKnacks = Knacks.insert skill "" character.skillKnacks }

        GainDomainKnack domain ->
            { character | domainKnacks = Knacks.insert domain "" character.domainKnacks }


applyBoons : List Boon -> Stats -> Stats
applyBoons boons character =
    List.foldl applyBoon character boons


addAbilities : List Ability -> Stats -> Stats
addAbilities abilities character =
    List.foldl (\ability -> applyBoons ability.boons)
        { character
            | abilities =
                List.foldl
                    (\ability -> Dict.insert ability.name ability)
                    character.abilities
                    abilities
        }
        abilities



-- ENCODE


encode : Stats -> Encode.Value
encode character =
    Encode.object
        [ ( "name", Encode.string character.name )
        , ( "class", Encode.string character.class )
        , ( "assignment", Encode.string character.assignment )
        , ( "skillKnacks", Knacks.encode character.skillKnacks )
        , ( "domainKnacks", Knacks.encode character.domainKnacks )
        , ( "equipment", Encode.string character.equipment )
        , ( "abilities", Encode.dict identity Ability.encode character.abilities )
        , ( "fallout", Encode.string character.fallout )
        , ( "refresh", Encode.string character.refresh )
        , ( "bonds", Encode.string character.bonds )
        , ( "skills", Skills.encode character.skills )
        , ( "domains", Domains.encode character.domains )
        , ( "resistances", Resistances.encode character.resistances )
        , ( "notes", Encode.string character.notes )
        ]



-- DECODER


decoder : Decode.Decoder Stats
decoder =
    Decode.succeed Stats
        |> Pipeline.optional "name" Decode.string ""
        |> Pipeline.optional "class" Decode.string ""
        |> Pipeline.optional "assignment" Decode.string ""
        |> Pipeline.optional "skills" Skills.decoder Skills.new
        |> Pipeline.optional "domains" Domains.decoder Domains.new
        |> Pipeline.optional "skillKnacks" Knacks.skillsDecoder Knacks.newSkills
        |> Pipeline.optional "domainKnacks" Knacks.domainsDecoder Knacks.newDomains
        |> Pipeline.optional "equipment" Decode.string ""
        |> Pipeline.optional "refresh" Decode.string ""
        |> Pipeline.optional "abilities" (Decode.dict Ability.decoder) Dict.empty
        |> Pipeline.optional "bonds" Decode.string ""
        |> Pipeline.optional "fallout" Decode.string ""
        |> Pipeline.optional "resistances" Resistances.decoder Resistances.new
        |> Pipeline.optional "notes" Decode.string ""


decodeLocalCharacter : String -> Stats
decodeLocalCharacter storedState =
    Result.withDefault blank (Decode.decodeString decoder storedState)



-- MSG


type Msg
    = UpdatedName String
    | UpdatedSkills Skills
    | UpdatedDomains Domains
    | UpdatedSkillKnacks (Knacks Skills.Skill)
    | UpdatedDomainKnacks (Knacks Domains.Domain)
    | UpdatedRefresh String
    | UpdatedEquipment String
    | UpdatedFallout String
    | UpdatedBonds String
    | UpdatedResistances Resistances
    | UpdatedNotes String
    | ClickedSave



-- UPDATE


update : Msg -> Stats -> Stats
update msg character =
    case msg of
        UpdatedName name ->
            { character | name = name }

        UpdatedSkills skills ->
            { character | skills = skills }

        UpdatedDomains domains ->
            { character | domains = domains }

        UpdatedSkillKnacks knacks ->
            { character | skillKnacks = knacks }

        UpdatedDomainKnacks knacks ->
            { character | domainKnacks = Debug.log "new knacks" knacks }

        UpdatedRefresh refresh ->
            { character | refresh = refresh }

        UpdatedEquipment equipment ->
            { character | equipment = equipment }

        UpdatedFallout fallout ->
            { character | fallout = fallout }

        UpdatedBonds bonds ->
            { character | bonds = bonds }

        UpdatedResistances resistances ->
            { character | resistances = resistances }

        UpdatedNotes notes ->
            { character | notes = notes }

        ClickedSave ->
            character



-- Debug.todo ""
-- VIEW


view : Stats -> Browser.Document Msg
view character =
    { title = character.name
    , body =
        [ main_
            [ class "wrapper gap-top-700"
            , class "character"
            ]
            [ section
                [ class "name" ]
                [ label
                    []
                    [ h1
                        []
                        [ text "Name" ]
                    , input [ type_ "text", value character.name, onInput UpdatedName ]
                        []
                    ]
                ]
            , section
                [ class "class" ]
                [ h2
                    []
                    [ text "Class" ]
                , div
                    [ class "text-500" ]
                    [ text character.class ]
                ]
            , section
                [ class "assignment" ]
                [ h2
                    []
                    [ text "Assignment" ]
                , div
                    [ class "text-500" ]
                    [ text character.assignment ]
                ]
            , section
                [ class "resistances" ]
                [ h2
                    []
                    [ text "Resistances" ]
                , Resistances.view UpdatedResistances character.resistances
                ]
            , div
                [ class "skills-and-domains" ]
                [ section
                    [ class "skills" ]
                    [ h2
                        []
                        [ a
                            [ href (Route.toString (Route.PlayAid PlayAids.Skills Nothing)) ]
                            [ text "Skills" ]
                        ]
                    , viewTraitList
                        UpdatedSkills
                        UpdatedSkillKnacks
                        character.skills
                        character.skillKnacks
                    ]
                , section
                    [ class "domains" ]
                    [ h2
                        []
                        [ a
                            [ href (Route.toString (Route.PlayAid PlayAids.Domains Nothing)) ]
                            [ text "Domains" ]
                        ]
                    , viewTraitList
                        UpdatedDomains
                        UpdatedDomainKnacks
                        character.domains
                        character.domainKnacks
                    ]
                ]
            , section
                [ class "flow"
                , class "abilities"
                ]
                [ h2
                    []
                    [ a
                        [ href (Route.toString (Route.Abilities (Just character.class))) ]
                        [ text "Abilities" ]
                    ]
                , ul
                    [ class "flow gap-top-500"
                    , class "abilities-list"
                    , attribute "role" "list"
                    ]
                    (List.map
                        (\ability ->
                            li []
                                [ Ability.viewCompact ability ]
                        )
                        (Dict.values character.abilities)
                    )
                ]
            , section
                [ class "fallout" ]
                [ h2
                    []
                    [ text "Fallout" ]
                , textarea
                    [ class "textbox"
                    , onInput UpdatedFallout
                    ]
                    [ text character.fallout ]
                ]
            , section
                [ class "equipment" ]
                [ h2
                    []
                    [ a
                        [ href (Route.toString (Route.PlayAid PlayAids.Weapons Nothing)) ]
                        [ text "Equipment" ]
                    ]
                , textarea
                    [ class "textbox"
                    , onInput UpdatedEquipment
                    ]
                    [ text character.equipment ]
                ]
            , section
                [ class "refresh" ]
                [ h2
                    []
                    [ text "Refresh" ]
                , textarea
                    [ class "textbox"
                    , onInput UpdatedRefresh
                    ]
                    [ text character.refresh ]
                ]
            , section
                [ class "bonds" ]
                [ h2
                    []
                    [ text "Bonds" ]
                , textarea
                    [ class "textbox"
                    , onInput UpdatedBonds
                    ]
                    [ text character.bonds ]
                ]

            -- , section
            --     [ class "knacks" ]
            --     [ h2
            --         []
            --         [ text "Knacks" ]
            --     , textarea
            --         [ class "textbox"
            --         , onInput UpdatedKnacks
            --         ]
            --         [ text character.knacks ]
            --     ]
            , section
                [ class "notes" ]
                [ h2
                    []
                    [ text "Notes" ]
                , textarea
                    [ class "textbox"
                    , onInput UpdatedNotes
                    ]
                    [ text character.notes ]
                ]
            , let
                domKs =
                    Debug.log "domain knacks"
                        (Knacks.newDomains
                            |> Knacks.insert Domains.Criminal "Hitmen"
                            |> Knacks.insert Domains.Weirdness "Wayward"
                            |> Knacks.insert Domains.Science "Chemistry"
                        )

                skillKs =
                    Knacks.newSkills
                        |> Knacks.insert Skills.Steal "Drugs"
                        |> Knacks.insert Skills.Steal "Weapons"
                        |> Knacks.insert Skills.Investigate "Murder"
                        |> Knacks.insert Skills.Scrap "Bladed weapons"
                        |> Knacks.insert Skills.Resist "Torture"
              in
              button
                [ onClick <| UpdatedDomainKnacks domKs
                , onClick <| UpdatedSkillKnacks skillKs
                ]
                [ text "gain knacks" ]
            ]
        ]
    }


viewTraitList :
    (TypeDict.Dict String k Bool -> msg)
    -> (TypeDict.Dict String k (Array String) -> msg)
    -> TypeDict.Dict String k Bool
    -> TypeDict.Dict String k (Array String)
    -> Html msg
viewTraitList toMsg knacksToMsg dict knacks =
    let
        list =
            TypeDict.toList dict

        labeller =
            TypeDict.toHasher dict

        labelId =
            labeller >> String.toLower >> (++) "cb-"

        viewItem ( k, isChecked ) =
            li [] <|
                div
                    []
                    [ input
                        [ id (labelId k)
                        , type_ "checkbox"
                        , checked isChecked
                        , onCheck (\b -> TypeDict.insert k b dict |> toMsg)
                        ]
                        []
                    , label
                        [ for (labelId k) ]
                        [ text (labeller k)
                        ]
                    , button
                        [ class "add-knack-button"
                        , onClick <| knacksToMsg (Knacks.insert k "" knacks)
                        ]
                        [ strong
                            []
                            [ text "+" ]
                        ]
                    ]
                    :: (case TypeDict.get k knacks of
                            Just ks ->
                                [ ul
                                    [ class "knacks__list"
                                    ]
                                  <|
                                    Array.toList <|
                                        Array.indexedMap
                                            (\idx knack ->
                                                li
                                                    []
                                                    [ input
                                                        [ value knack
                                                        , onInput
                                                            (\newKnack ->
                                                                knacksToMsg <| Knacks.update k idx newKnack knacks
                                                            )
                                                        ]
                                                        []
                                                    ]
                                            )
                                            ks
                                ]

                            Nothing ->
                                []
                       )
    in
    ul
        [ attribute "role" "list"
        , class "checkbox-list"
        ]
        (List.map viewItem list)
