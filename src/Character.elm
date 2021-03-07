module Character exposing (Domain(..), Resistance(..), Skill(..), Stats, blank, decodeLocalCharacter, decoder, encode, view)

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
    , abilities : String
    , bonds : String
    , fallout : String
    , resistances : Resistances
    }


blank : Stats
blank =
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



-- ENCODE


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
    Result.withDefault blank (Decode.decodeString decoder storedState)


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
                [ viewResistances (\resistances -> toMsg { character | resistances = resistances }) character.resistances
                ]
            , div
                [ class "skills-and-domains" ]
                [ section
                    [ class "skills" ]
                    [ h2
                        []
                        [ text "Skills" ]
                    , viewBoolDict (\skills -> toMsg { character | skills = skills }) skillToString character.skills
                    ]
                , section
                    [ class "domains" ]
                    [ h2
                        []
                        [ text "Domains" ]
                    , viewBoolDict (\domains -> toMsg { character | domains = domains }) domainToString character.domains
                    ]
                ]
            , section
                [ class "abilities" ]
                [ h2
                    []
                    [ text "Abilities" ]
                , textarea
                    [ onInput (\abilities -> toMsg { character | abilities = abilities }) ]
                    [ text character.abilities ]
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
