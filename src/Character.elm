module Character exposing (Msg(..), Stats, addAbilities, applyAssignment, applyBoons, applyClass, blank, decodeLocalCharacter, decoder, encode, save, update, view)

-- import Ability exposing (Ability)

import Ability exposing (Ability)
import Boon exposing (Boon(..))
import Boon.Domain as Domains exposing (Domains)
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
    , knacks = ""
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
            { character | resistances = TypedDict.update resistance ((+) bonus >> clamp 0 5) character.resistances }

        GainDomains domains ->
            { character | domains = TypedDict.setAll (List.map (\d -> ( d, True )) domains) character.domains }

        GainSkills skills ->
            { character | skills = TypedDict.setAll (List.map (\s -> ( s, True )) skills) character.skills }

        GainEquipment equipment ->
            { character | equipment = character.equipment ++ String.join "\n\n" equipment ++ "\n\n" }

        GainRefresh refresh ->
            { character | refresh = character.refresh ++ String.join "\n\n" refresh ++ "\n\n" }


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
        , ( "knacks", Encode.string character.knacks )
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
        |> Pipeline.optional "knacks" Decode.string ""
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
    | UpdatedKnacks String
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

        UpdatedKnacks knacks ->
            { character | knacks = knacks }

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
            [ class "wrapper"
            , class "character"
            ]
            [ section
                [ class "name" ]
                [ label
                    []
                    [ h2
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
                    [ class "font-size-700" ]
                    [ text character.class ]
                ]
            , section
                [ class "assignment" ]
                [ h2
                    []
                    [ text "Assignment" ]
                , div
                    [ class "font-size-700" ]
                    [ text character.assignment ]
                ]
            , section
                [ class "resistances" ]
                [ Resistances.view UpdatedResistances character.resistances
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
                    , viewBoolDict UpdatedSkills Skills.toString character.skills
                    ]
                , section
                    [ class "domains" ]
                    [ h2
                        []
                        [ a
                            [ href (Route.toString (Route.PlayAid PlayAids.Domains Nothing)) ]
                            [ text "Domains" ]
                        ]
                    , viewBoolDict UpdatedDomains Domains.toString character.domains
                    ]
                ]
            , section
                [ class "abilities" ]
                [ h2
                    []
                    [ a
                        [ href (Route.toString (Route.Abilities (Just character.class))) ]
                        [ text "Abilities" ]
                    ]
                , ul
                    []
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
            , section
                [ class "knacks" ]
                [ h2
                    []
                    [ text "Knacks" ]
                , textarea
                    [ class "textbox"
                    , onInput UpdatedKnacks
                    ]
                    [ text character.knacks ]
                ]
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
