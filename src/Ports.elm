port module Ports exposing (confirmLocalStorage, savedCharacter, storeCharacter, updatedCharacter)


port storeCharacter : String -> Cmd msg


port savedCharacter : () -> Cmd msg


port updatedCharacter : () -> Cmd msg


port confirmLocalStorage : (() -> msg) -> Sub msg



-- port savedChanges : (() -> msg) -> Sub msg
