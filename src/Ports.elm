port module Ports exposing (savedCharacter, storeCharacter, updatedCharacter)


port storeCharacter : String -> Cmd msg


port savedCharacter : () -> Cmd msg


port updatedCharacter : () -> Cmd msg



-- port savedChanges : (() -> msg) -> Sub msg
