module Session exposing (Session, changes, changesToString, character, decoder, fromDisk, isSaved, isSavedLocally, isUnsaved, navKey, new, save, savedChanges, savedChangesLocally, setCharacter)

import Browser.Navigation as Nav
import Character
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Ports


type Session
    = Character
        { navKey : Nav.Key
        , character : Character.Stats
        , changes : Changes
        }
    | NoCharacter Nav.Key


type Changes
    = Unsaved
    | Saved
    | SavedLocally


isSaved : Session -> Bool
isSaved =
    changes >> (==) Saved


isSavedLocally : Session -> Bool
isSavedLocally =
    changes >> (==) SavedLocally


isUnsaved : Session -> Bool
isUnsaved =
    changes >> (==) Unsaved


changesToString : Changes -> String
changesToString val =
    case val of
        Unsaved ->
            "There are unsaved changes."

        SavedLocally ->
            "All changes saved locally."

        Saved ->
            "All changes saved to disk."


new : Nav.Key -> Session
new key =
    NoCharacter key


navKey : Session -> Nav.Key
navKey session =
    case session of
        Character val ->
            val.navKey

        NoCharacter val ->
            val


character : Session -> Maybe Character.Stats
character session =
    case session of
        Character val ->
            Just val.character

        NoCharacter _ ->
            Nothing


setCharacter : Character.Stats -> Session -> Session
setCharacter newCharacter session =
    case session of
        NoCharacter key ->
            Character
                { navKey = key
                , character = newCharacter
                , changes = Unsaved
                }

        Character val ->
            Character
                { val
                    | character = newCharacter
                    , changes = Unsaved
                }


fromDisk : Character.Stats -> Session -> Session
fromDisk newCharacter session =
    case session of
        NoCharacter key ->
            Character
                { navKey = key
                , character = newCharacter
                , changes = Saved
                }

        Character val ->
            Character
                { val
                    | character = newCharacter
                    , changes = Saved
                }


changes : Session -> Changes
changes session =
    case session of
        Character val ->
            val.changes

        NoCharacter _ ->
            Saved


savedChanges : Session -> Session
savedChanges session =
    case session of
        NoCharacter _ ->
            session

        Character val ->
            Character { val | changes = Saved }


savedChangesLocally : Session -> Session
savedChangesLocally session =
    case session of
        NoCharacter _ ->
            session

        Character val ->
            Character
                { val
                    | changes =
                        case val.changes of
                            Unsaved ->
                                SavedLocally

                            SavedLocally ->
                                SavedLocally

                            Saved ->
                                Saved
                }


save : Session -> ( Session, Cmd msg )
save session =
    case session of
        NoCharacter _ ->
            ( session, Cmd.none )

        Character _ ->
            let
                updatedSession =
                    savedChangesLocally session
            in
            ( savedChangesLocally updatedSession
            , Encode.encode 2 (encoder updatedSession)
                |> Ports.storeCharacter
            )



-- ENCODER


encoder : Session -> Value
encoder session =
    case session of
        NoCharacter _ ->
            Encode.null

        Character val ->
            Encode.object
                [ ( "character", Character.encode val.character )
                , ( "changes", encodeChanges val.changes )
                ]


encodeChanges : Changes -> Value
encodeChanges cs =
    Encode.int <|
        case cs of
            Unsaved ->
                0

            SavedLocally ->
                1

            Saved ->
                2



-- DECODER


decoder : Nav.Key -> Decoder Session
decoder key =
    Decode.map
        (Maybe.withDefault (NoCharacter key))
        (Decode.nullable (Decode.map Character (decodeCharacterSession key)))


decodeCharacterSession :
    Nav.Key
    ->
        Decoder
            { navKey : Nav.Key
            , character : Character.Stats
            , changes : Changes
            }
decodeCharacterSession key =
    Decode.map2
        (\char cs ->
            { navKey = key
            , character = char
            , changes = cs
            }
        )
        (Decode.field "character" Character.decoder)
        (Decode.field "changes" changesDecoder)


changesDecoder : Decoder Changes
changesDecoder =
    Decode.andThen
        (\n ->
            case n of
                0 ->
                    Decode.succeed Unsaved

                1 ->
                    Decode.succeed SavedLocally

                2 ->
                    Decode.succeed Saved

                _ ->
                    Decode.fail <| "Invalid changes value: " ++ String.fromInt n
        )
        Decode.int
