module Ability exposing (..)

import Boon exposing (Boon)


type alias Ability =
    { name : String
    , flavor : Maybe String
    , boons : List Boon
    , text : String
    }
