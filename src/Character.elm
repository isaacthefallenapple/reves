module Character exposing (Msg(..), Stats, applyAssignment, applyBoons, applyClass, blank, decodeLocalCharacter, decoder, encode, update, view)

-- import Ability exposing (Ability)

import Ability exposing (Ability)
import Boon exposing (Boon(..))
import Boon.Domain as Domains exposing (Domains)
import Boon.Resistance as Resistances exposing (Resistances)
import Boon.Skill as Skills exposing (Skills)
import Browser
import Class exposing (Class)
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


applyClass : Class -> Stats -> Stats
applyClass { name, boons, coreAbilities } character =
    applyBoons boons
        { character
            | class = name
            , abilities = character.abilities ++ coreAbilities
        }


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



-- ENCODE


encode : Stats -> Encode.Value
encode character =
    Encode.object
        [ ( "name", Encode.string character.name )
        , ( "class", Encode.string character.class )
        , ( "assignment", Encode.string character.assignment )
        , ( "knacks", Encode.string character.knacks )
        , ( "equipment", Encode.string character.equipment )
        , ( "abilities", Encode.list Ability.encode character.abilities )
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
        |> Pipeline.optional "abilities" (Decode.list Ability.decoder) []
        |> Pipeline.optional "bonds" Decode.string ""
        |> Pipeline.optional "fallout" Decode.string ""
        |> Pipeline.optional "resistances" Resistances.decoder Resistances.new


decodeLocalCharacter : String -> Stats
decodeLocalCharacter storedState =
    Result.withDefault blank (Decode.decodeString decoder storedState)



-- MSG


type Msg
    = Updated CharacterUpdateMsg
    | ClickedViewAbilities


type CharacterUpdateMsg
    = UpdatedName String
    | UpdatedSkills Skills
    | UpdatedDomains Domains
    | UpdatedKnacks String
    | UpdatedRefresh String
    | UpdatedEquipment String
    | UpdatedFallout String
    | UpdatedBonds String
    | UpdatedResistances Resistances



-- UPDATE


update : CharacterUpdateMsg -> Stats -> Stats
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



-- Debug.todo ""
-- VIEW


view : (Msg -> msg) -> Stats -> Browser.Document msg
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
                    , input [ type_ "text", value character.name, onInput (UpdatedName >> Updated >> toMsg) ]
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
                    , input [ type_ "text", value character.class ]
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
                    , input [ type_ "text", value character.assignment ]
                        []
                    ]
                ]
            , section
                [ class "resistances" ]
                [ Resistances.view (UpdatedResistances >> Updated >> toMsg) character.resistances
                ]
            , div
                [ class "skills-and-domains" ]
                [ section
                    [ class "skills" ]
                    [ h2
                        []
                        [ text "Skills" ]
                    , viewBoolDict (UpdatedSkills >> Updated >> toMsg) Skills.toString character.skills
                    ]
                , section
                    [ class "domains" ]
                    [ h2
                        []
                        [ text "Domains" ]
                    , viewBoolDict (UpdatedDomains >> Updated >> toMsg) Domains.toString character.domains
                    ]
                ]
            , section
                [ class "abilities" ]
                [ h2
                    []
                    [ text "Abilities" ]
                , button
                    [ onClick (toMsg ClickedViewAbilities) ]
                    [ text "view" ]
                , ul
                    []
                    (List.map
                        (\ability ->
                            li []
                                [ Ability.view ability ]
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
                    [ onInput (UpdatedFallout >> Updated >> toMsg) ]
                    [ text character.fallout ]
                ]
            , section
                [ class "equipment" ]
                [ h2
                    []
                    [ text "Equipment" ]
                , textarea
                    [ onInput (UpdatedEquipment >> Updated >> toMsg) ]
                    [ text character.equipment ]
                ]
            , section
                [ class "refresh" ]
                [ h2
                    []
                    [ text "Refresh" ]
                , textarea
                    [ onInput (UpdatedRefresh >> Updated >> toMsg) ]
                    [ text character.refresh ]
                ]
            , section
                [ class "bonds" ]
                [ h2
                    []
                    [ text "Bonds" ]
                , textarea
                    [ onInput (UpdatedBonds >> Updated >> toMsg) ]
                    [ text character.bonds ]
                ]
            , section
                [ class "knacks" ]
                [ h2
                    []
                    [ text "Knacks" ]
                , textarea
                    [ onInput (UpdatedKnacks >> Updated >> toMsg) ]
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
