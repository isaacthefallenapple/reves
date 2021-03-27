port module Ports exposing (savedChanges, storeCharacter)


port storeCharacter : String -> Cmd msg


port savedChanges : (() -> msg) -> Sub msg
