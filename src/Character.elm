module Character exposing (Stats, applyBoons, blank, decodeLocalCharacter, decoder, encode, view)

-- import Ability exposing (Ability)

import Boon exposing (Ability, Boon(..))
import Boon.Domain as Domains exposing (Domains)
import Boon.Resistance as Resistances exposing (Resistances)
import Boon.Skill as Skills exposing (Skills)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import TypedDict exposing (TypedDict)


type alias Stats =
    { name : String
    , class : String
    , assignment : String
    , skills : Skills
    , domains : Domains
    , knacks : String
    , equipment : String
    , refresh : String
    , abilities : List Ability
    , bonds : String
    , fallout : String
    , resistances : Resistances
    }


blank : Stats
blank =
    { name = ""
    , class = ""
    , assignment = ""
    , skills = Skills.new
    , domains = Domains.new
    , knacks = ""
    , equipment = ""
    , refresh = ""
    , abilities = []
    , bonds = ""
    , fallout = ""
    , resistances = Resistances.new
    }


applyBoon : Boon -> Stats -> Stats
applyBoon boon character =
    case boon of
        ResistanceUp resistance bonus ->
            { character | resistances = TypedDict.update resistance ((+) bonus >> clamp 0 5) character.resistances }

        NewDomain domain ->
            { character | domains = TypedDict.set domain True character.domains }

        NewSkill skill ->
            { character | skills = TypedDict.set skill True character.skills }

        NewAbility ability ->
            applyBoons ability.boons
                { character
                    | abilities =
                        character.abilities
                            ++ [ ability ]
                }

        NewEquipment equipment ->
            { character | equipment = character.equipment ++ "\n\n" ++ equipment }

        NewRefresh refresh ->
            { character | refresh = character.refresh ++ "\n\n" ++ refresh }


applyBoons : List Boon -> Stats -> Stats
applyBoons boons character =
    List.foldl applyBoon character boons



-- ENCODE


encode : Stats -> Encode.Value
encode character =
    Encode.object
        [ ( "name", Encode.string character.name )
        , ( "class", Encode.string character.class )
        , ( "assignment", Encode.string character.assignment )
        , ( "knacks", Encode.string character.knacks )
        , ( "equipment", Encode.string character.equipment )
        , ( "abilities", Encode.list Boon.encodeAbility character.abilities )
        , ( "fallout", Encode.string character.fallout )
        , ( "refresh", Encode.string character.refresh )
        , ( "bonds", Encode.string character.bonds )
        , ( "skills", Skills.encode character.skills )
        , ( "domains", Domains.encode character.domains )
        , ( "resistances", Resistances.encode character.resistances )
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
        |> Pipeline.optional "knacks" Decode.string ""
        |> Pipeline.optional "equipment" Decode.string ""
        |> Pipeline.optional "refresh" Decode.string ""
        |> Pipeline.optional "abilities" (Decode.list Boon.abilityDecoder) []
        |> Pipeline.optional "bonds" Decode.string ""
        |> Pipeline.optional "fallout" Decode.string ""
        |> Pipeline.optional "resistances" Resistances.decoder Resistances.new


decodeLocalCharacter : String -> Stats
decodeLocalCharacter storedState =
    Result.withDefault blank (Decode.decodeString decoder storedState)



-- VIEW


view : (Stats -> msg) -> Stats -> Browser.Document msg
view toMsg character =
    { title = character.name
    , body =
        [ main_
            []
            [ section
                [ class "name" ]
                [ label
                    []
                    [ h2
                        []
                        [ text "Name" ]
                    , input [ type_ "text", value character.name, onInput (\name -> toMsg { character | name = name }) ]
                        []
                    ]
                ]
            , section
                [ class "class" ]
                [ label
                    []
                    [ h2
                        []
                        [ text "Class" ]
                    , input [ type_ "text", value character.class, onInput (\class -> toMsg { character | class = class }) ]
                        []
                    ]
                ]
            , section
                [ class "assignment" ]
                [ label
                    []
                    [ h2
                        []
                        [ text "Assignment" ]
                    , input [ type_ "text", value character.assignment, onInput (\assignment -> toMsg { character | assignment = assignment }) ]
                        []
                    ]
                ]
            , section
                [ class "resistances" ]
                [ Resistances.view (\resistances -> toMsg { character | resistances = resistances }) character.resistances
                ]
            , div
                [ class "skills-and-domains" ]
                [ section
                    [ class "skills" ]
                    [ h2
                        []
                        [ text "Skills" ]
                    , viewBoolDict (\skills -> toMsg { character | skills = skills }) Skills.toString character.skills
                    ]
                , section
                    [ class "domains" ]
                    [ h2
                        []
                        [ text "Domains" ]
                    , viewBoolDict (\domains -> toMsg { character | domains = domains }) Domains.toString character.domains
                    ]
                ]
            , section
                [ class "abilities" ]
                [ h2
                    []
                    [ text "Abilities" ]
                , ul
                    []
                    (List.map
                        (\ability ->
                            li []
                                [ Boon.viewAbility ability ]
                        )
                        character.abilities
                    )
                ]
            , section
                [ class "fallout" ]
                [ h2
                    []
                    [ text "Fallout" ]
                , textarea
                    [ onInput (\fallout -> toMsg { character | fallout = fallout }) ]
                    [ text character.fallout ]
                ]
            , section
                [ class "equipment" ]
                [ h2
                    []
                    [ text "Equipment" ]
                , textarea
                    [ onInput (\equipment -> toMsg { character | equipment = equipment }) ]
                    [ text character.equipment ]
                ]
            , section
                [ class "refresh" ]
                [ h2
                    []
                    [ text "Refresh" ]
                , textarea
                    [ onInput (\refresh -> toMsg { character | refresh = refresh }) ]
                    [ text character.refresh ]
                ]
            , section
                [ class "bonds" ]
                [ h2
                    []
                    [ text "Bonds" ]
                , textarea
                    [ onInput (\bonds -> toMsg { character | bonds = bonds }) ]
                    [ text character.bonds ]
                ]
            , section
                [ class "knacks" ]
                [ h2
                    []
                    [ text "Knacks" ]
                , textarea
                    [ onInput (\knacks -> toMsg { character | knacks = knacks }) ]
                    [ text character.knacks ]
                ]
            ]
        ]
    }


viewBoolDict : (TypedDict k Bool -> msg) -> (k -> String) -> TypedDict k Bool -> Html msg
viewBoolDict toMsg labeller dict =
    let
        list =
            TypedDict.unwrap dict

        viewItem ( k, isChecked ) =
            li []
                [ label []
                    [ input
                        [ type_ "checkbox"
                        , checked isChecked
                        , onCheck (\b -> TypedDict.set k b dict |> toMsg)
                        ]
                        []
                    , text (labeller k)
                    ]
                ]
    in
    ul
        []
        (List.map viewItem list)
