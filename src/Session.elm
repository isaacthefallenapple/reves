module Session exposing (Session, changes, changesToString, character, load, navKey, new, save, savedChanges, savedChangesLocally, setCharacter)

import Browser.Navigation as Nav
import Character
import Json.Encode as Encode
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


load : Nav.Key -> Character.Stats -> Session
load key char =
    Character
        { navKey = key
        , character = char
        , changes = Saved
        }


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
            Character { val | changes = SavedLocally }


save : Session -> Cmd msg
save session =
    case session of
        NoCharacter _ ->
            Cmd.none

        Character val ->
            Encode.encode 2 (Character.encode val.character)
                |> Ports.storeCharacter
