module Session exposing (Session, character, load, navKey, new, save, savedChanges, setCharacter, unsavedChanges)

import Browser.Navigation as Nav
import Character
import Json.Encode as Encode
import Ports


type Session
    = Character
        { navKey : Nav.Key
        , character : Character.Stats
        , unsavedChanges : Bool
        }
    | NoCharacter Nav.Key


new : Nav.Key -> Session
new key =
    NoCharacter key


load : Nav.Key -> Character.Stats -> Session
load key char =
    Character
        { navKey = key
        , character = char
        , unsavedChanges = False
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
                , unsavedChanges = True
                }

        Character val ->
            Character
                { val
                    | character = newCharacter
                    , unsavedChanges = True
                }


unsavedChanges : Session -> Bool
unsavedChanges session =
    case session of
        Character val ->
            val.unsavedChanges

        NoCharacter _ ->
            False


savedChanges : Session -> Session
savedChanges session =
    case session of
        NoCharacter _ ->
            session

        Character val ->
            Character { val | unsavedChanges = False }


save : Session -> Cmd msg
save session =
    case session of
        NoCharacter _ ->
            Cmd.none

        Character val ->
            Encode.encode 2 (Character.encode val.character)
                |> Ports.storeCharacter
