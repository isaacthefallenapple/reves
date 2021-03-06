module Main exposing (main)

import Browser
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Maybe
import Ports
import Task
import TypedDict exposing (TypedDict)



-- TYPES


type Model
    = Character Stats
    | DecodeErr Decode.Error


type alias Stats =
    { name : String
    , class : String
    , assignment : String
    , skills : Skills
    , domains : Domains
    , knacks : String
    , equipment : String
    , refresh : String
    , abilities : String
    , bonds : String
    , fallout : String
    , resistances : Resistances
    }


type alias Resistances =
    TypedDict Resistance Int


newResistances : Resistances
newResistances =
    TypedDict.fromListWithDefault 0
        [ Body
        , Resolve
        , Resources
        , Shadow
        , Reputation
        ]


type Resistance
    = Body
    | Resolve
    | Resources
    | Shadow
    | Reputation


resistanceToString : Resistance -> String
resistanceToString resistance =
    case resistance of
        Body ->
            "Body"

        Resolve ->
            "Resolve"

        Resources ->
            "Resources"

        Shadow ->
            "Shadow"

        Reputation ->
            "Reputation"


stringToResistance : String -> Maybe Resistance
stringToResistance s =
    case s of
        "Body" ->
            Just Body

        "Resolve" ->
            Just Resolve

        "Resources" ->
            Just Resources

        "Shadow" ->
            Just Shadow

        "Reputation" ->
            Just Reputation

        _ ->
            Nothing


blankCharacter : Stats
blankCharacter =
    { name = ""
    , class = ""
    , assignment = ""
    , skills = newSkills
    , domains = newDomains
    , knacks = ""
    , equipment = ""
    , refresh = ""
    , abilities = ""
    , bonds = ""
    , fallout = ""
    , resistances = newResistances
    }


type Skill
    = Compel
    | Deceive
    | Hack
    | Investigate
    | Patch
    | Resist
    | Scramble
    | Scrap
    | Skulk
    | Steal


skillToString : Skill -> String
skillToString skill =
    case skill of
        Compel ->
            "Compel"

        Deceive ->
            "Deceive"

        Hack ->
            "Hack"

        Patch ->
            "Patch"

        Scramble ->
            "Scramble"

        Scrap ->
            "Scrap"

        Skulk ->
            "Skulk"

        Investigate ->
            "Investigate"

        Steal ->
            "Steal"

        Resist ->
            "Resist"


stringToSkill : String -> Maybe Skill
stringToSkill s =
    case s of
        "Compel" ->
            Just Compel

        "Deceive" ->
            Just Deceive

        "Hack" ->
            Just Hack

        "Patch" ->
            Just Patch

        "Scramble" ->
            Just Scramble

        "Scrap" ->
            Just Scrap

        "Skulk" ->
            Just Skulk

        "Investigate" ->
            Just Investigate

        "Steal" ->
            Just Steal

        "Resist" ->
            Just Resist

        _ ->
            Nothing


type Domain
    = Criminal
    | HighSociety
    | LowSociety
    | Weirdness
    | Hegemony
    | Science


domainToString : Domain -> String
domainToString domain =
    case domain of
        Criminal ->
            "Criminal"

        HighSociety ->
            "High Society"

        LowSociety ->
            "Low Society"

        Weirdness ->
            "Weirdness"

        Hegemony ->
            "Hegemony"

        Science ->
            "Science"


stringToDomain : String -> Maybe Domain
stringToDomain s =
    case s of
        "Criminal" ->
            Just Criminal

        "High Society" ->
            Just HighSociety

        "Low Society" ->
            Just LowSociety

        "Weirdness" ->
            Just Weirdness

        "Hegemony" ->
            Just Hegemony

        "Science" ->
            Just Science

        _ ->
            Nothing


type alias Skills =
    TypedDict Skill Bool


newSkills : Skills
newSkills =
    TypedDict.fromListWithDefault False
        [ Compel
        , Deceive
        , Hack
        , Patch
        , Scramble
        , Scrap
        , Skulk
        , Investigate
        , Steal
        , Resist
        ]


newDomains : Domains
newDomains =
    TypedDict.fromListWithDefault False
        [ Criminal
        , HighSociety
        , LowSociety
        , Weirdness
        , Hegemony
        , Science
        ]


type alias Domains =
    TypedDict Domain Bool


type Boon
    = ResistanceUp Resistance Int
    | NewDomain Domain
    | NewSkill Skill
    | NewAbility String
    | NewEquipment String
    | NewRefresh String


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
            { character | abilities = character.abilities ++ "\n\n" ++ ability }

        NewEquipment equipment ->
            { character | equipment = character.equipment ++ "\n\n" ++ equipment }

        NewRefresh refresh ->
            { character | refresh = character.refresh ++ "\n\n" ++ refresh }


applyBoons : List Boon -> Stats -> Stats
applyBoons boons character =
    List.foldl applyBoon character boons



-- MSG


type Msg
    = UpdatedName String
    | UpdatedClass String
    | UpdatedAssignment String
    | UpdatedSkills Skills
    | UpdatedDomains Domains
    | UpdatedKnacks String
    | UpdatedEquipment String
    | UpdatedRefresh String
    | UpdatedAbilities String
    | UpdatedBonds String
    | UpdatedFallout String
    | UpdatedResistances Resistances
    | ClickedOpenFile
    | FileLoaded File
    | ReadFile String
    | ClickedSave



-- ENCODE


saveCharacter : Stats -> Cmd msg
saveCharacter character =
    Encode.encode 2 (encode character) |> Ports.storeCharacter


encode : Stats -> Encode.Value
encode character =
    Encode.object
        [ ( "name", Encode.string character.name )
        , ( "class", Encode.string character.class )
        , ( "assignment", Encode.string character.assignment )
        , ( "knacks", Encode.string character.knacks )
        , ( "equipment", Encode.string character.equipment )
        , ( "abilities", Encode.string character.abilities )
        , ( "fallout", Encode.string character.fallout )
        , ( "refresh", Encode.string character.refresh )
        , ( "bonds", Encode.string character.bonds )
        , ( "skills", encodeSkills character.skills )
        , ( "domains", encodeDomains character.domains )
        , ( "resistances", encodeResistances character.resistances )
        ]


encodeResistances : Resistances -> Encode.Value
encodeResistances =
    TypedDict.encode resistanceToString Encode.int


encodeSkills : Skills -> Encode.Value
encodeSkills =
    encodeBoolDict skillToString


encodeDomains : Domains -> Encode.Value
encodeDomains =
    encodeBoolDict domainToString


encodeBoolDict : (k -> String) -> TypedDict k Bool -> Encode.Value
encodeBoolDict stringer dict =
    let
        list =
            TypedDict.unwrap dict
    in
    Encode.object (List.map (Tuple.mapBoth stringer Encode.bool) list)



-- DECODER


decoder : Decode.Decoder Stats
decoder =
    Decode.succeed Stats
        |> Pipeline.optional "name" Decode.string ""
        |> Pipeline.optional "class" Decode.string ""
        |> Pipeline.optional "assignment" Decode.string ""
        |> Pipeline.optional "skills" skillsDecoder newSkills
        |> Pipeline.optional "domains" domainsDecoder newDomains
        |> Pipeline.optional "knacks" Decode.string ""
        |> Pipeline.optional "equipment" Decode.string ""
        |> Pipeline.optional "refresh" Decode.string ""
        |> Pipeline.optional "abilities" Decode.string ""
        |> Pipeline.optional "bonds" Decode.string ""
        |> Pipeline.optional "fallout" Decode.string ""
        |> Pipeline.optional "resistances" resistancesDecoder newResistances


decodeLocalCharacter : String -> Stats
decodeLocalCharacter storedState =
    Result.withDefault blankCharacter (Decode.decodeString decoder storedState)


skillsDecoder : Decode.Decoder Skills
skillsDecoder =
    TypedDict.decoder (stringToSkill >> Maybe.withDefault Compel) Decode.bool


domainsDecoder : Decode.Decoder Domains
domainsDecoder =
    TypedDict.decoder (stringToDomain >> Maybe.withDefault LowSociety) Decode.bool


resistancesDecoder : Decode.Decoder Resistances
resistancesDecoder =
    TypedDict.decoder (stringToResistance >> Maybe.withDefault Body) Decode.int



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model of
        DecodeErr err ->
            { title = "Error"
            , body =
                [ h1
                    []
                    [ text "Something went wrong :("
                    ]
                , p
                    []
                    [ text (Decode.errorToString err) ]
                , p
                    []
                    [ button
                        [ onClick ClickedOpenFile ]
                        [ text "Try a different file" ]
                    ]
                ]
            }

        Character character ->
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
                            , input [ type_ "text", value character.name, onInput UpdatedName ]
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
                            , input [ type_ "text", value character.class, onInput UpdatedClass ]
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
                            , input [ type_ "text", value character.assignment, onInput UpdatedAssignment ]
                                []
                            ]
                        ]
                    , section
                        [ class "resistances" ]
                        [ viewResistances UpdatedResistances character.resistances
                        ]
                    , div
                        [ class "skills-and-domains" ]
                        [ section
                            [ class "skills" ]
                            [ h2
                                []
                                [ text "Skills" ]
                            , viewBoolDict UpdatedSkills skillToString character.skills
                            ]
                        , section
                            [ class "domains" ]
                            [ h2
                                []
                                [ text "Domains" ]
                            , viewBoolDict UpdatedDomains domainToString character.domains
                            ]
                        ]
                    , section
                        [ class "abilities" ]
                        [ h2
                            []
                            [ text "Abilities" ]
                        , textarea
                            [ onInput UpdatedAbilities ]
                            [ text character.abilities ]
                        ]
                    , section
                        [ class "fallout" ]
                        [ h2
                            []
                            [ text "Fallout" ]
                        , textarea
                            [ onInput UpdatedFallout ]
                            [ text character.fallout ]
                        ]
                    , section
                        [ class "equipment" ]
                        [ h2
                            []
                            [ text "Equipment" ]
                        , textarea
                            [ onInput UpdatedEquipment ]
                            [ text character.equipment ]
                        ]
                    , section
                        [ class "refresh" ]
                        [ h2
                            []
                            [ text "Refresh" ]
                        , textarea
                            [ onInput UpdatedRefresh ]
                            [ text character.refresh ]
                        ]
                    , section
                        [ class "bonds" ]
                        [ h2
                            []
                            [ text "Bonds" ]
                        , textarea
                            [ onInput UpdatedBonds ]
                            [ text character.bonds ]
                        ]
                    , section
                        [ class "knacks" ]
                        [ h2
                            []
                            [ text "Knacks" ]
                        , textarea
                            [ onInput UpdatedKnacks ]
                            [ text character.knacks ]
                        ]
                    ]
                , footer
                    [ class "io" ]
                    [ button
                        [ onClick ClickedOpenFile ]
                        [ text "Open" ]
                    , button
                        [ onClick ClickedSave ]
                        [ text "Save" ]
                    ]
                ]
            }


viewResistances : (Resistances -> msg) -> Resistances -> Html msg
viewResistances toMsg resistances =
    let
        resistancesList =
            [ Body, Resolve, Resources, Shadow, Reputation ]
    in
    table
        []
        [ thead
            []
            [ tr
                []
                (List.map (resistanceToString >> text >> List.singleton >> th [])
                    resistancesList
                )
            ]
        , tbody
            []
            [ tr
                []
                (List.map
                    (\r ->
                        td
                            []
                            [ input
                                [ type_ "number"
                                , value
                                    (String.fromInt
                                        (Maybe.withDefault 0 (TypedDict.get r resistances))
                                    )
                                , onInput
                                    (\s ->
                                        toMsg
                                            (TypedDict.set r
                                                (s
                                                    |> String.toInt
                                                    |> Maybe.withDefault 0
                                                    |> clamp 0 5
                                                )
                                                resistances
                                            )
                                    )
                                ]
                                []
                            ]
                    )
                    resistancesList
                )
            ]
        ]


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



-- UPDATE


updateCharacter : ( Stats, Cmd Msg ) -> ( Model, Cmd Msg )
updateCharacter ( character, msg ) =
    ( Character character, Cmd.batch [ msg, saveCharacter character ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Character character, UpdatedName name ) ->
            ( { character | name = name }, Cmd.none )
                |> updateCharacter

        ( Character character, UpdatedClass class ) ->
            ( { character | class = class }, Cmd.none )
                |> updateCharacter

        ( Character character, UpdatedAssignment assignment ) ->
            ( { character | assignment = assignment }, Cmd.none )
                |> updateCharacter

        ( Character character, UpdatedSkills skills ) ->
            ( { character | skills = skills }, Cmd.none )
                |> updateCharacter

        ( Character character, UpdatedDomains domains ) ->
            ( { character | domains = domains }, Cmd.none )
                |> updateCharacter

        ( Character character, UpdatedKnacks knacks ) ->
            ( { character | knacks = knacks }, Cmd.none )
                |> updateCharacter

        ( Character character, UpdatedEquipment equipment ) ->
            ( { character | equipment = equipment }, Cmd.none )
                |> updateCharacter

        ( Character character, UpdatedRefresh refresh ) ->
            ( { character | refresh = refresh }, Cmd.none )
                |> updateCharacter

        ( Character character, UpdatedAbilities abilities ) ->
            ( { character | abilities = abilities }, Cmd.none )
                |> updateCharacter

        ( Character character, UpdatedBonds bonds ) ->
            ( { character | bonds = bonds }, Cmd.none )
                |> updateCharacter

        ( Character character, UpdatedFallout fallout ) ->
            ( { character | fallout = fallout }, Cmd.none )
                |> updateCharacter

        ( Character character, UpdatedResistances resistances ) ->
            ( { character | resistances = resistances }, Cmd.none )
                |> updateCharacter

        ( Character character, ClickedSave ) ->
            ( model, Download.string character.name "application/json" (Encode.encode 2 (encode character)) )

        ( _, ClickedOpenFile ) ->
            ( model, Select.file [ "application/json" ] FileLoaded )

        ( _, FileLoaded file ) ->
            ( model, Task.perform ReadFile (File.toString file) )

        ( _, ReadFile content ) ->
            ( case Decode.decodeString decoder content of
                Err err ->
                    DecodeErr err

                Ok character ->
                    Character character
            , Cmd.none
            )

        ( _, _ ) ->
            ( model, Cmd.none )



-- INIT


init : Maybe String -> ( Model, Cmd Msg )
init flags =
    ( Character (Maybe.withDefault blankCharacter (Maybe.map decodeLocalCharacter flags))
    , Cmd.none
    )



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MAIN


main : Program (Maybe String) Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
