module Status exposing (HttpStatus, Status(..), fromResult, map, mapErr, withDefault)

{-| Just a shitty version of RemoteData
-}

import Http


type Status err data
    = Loading
    | Loaded data
    | Failed err


type alias HttpStatus data =
    Status Http.Error data


map : (a -> b) -> Status x a -> Status x b
map f status =
    case status of
        Loaded a ->
            Loaded (f a)

        Loading ->
            Loading

        Failed x ->
            Failed x


mapErr : (x -> y) -> Status x a -> Status y a
mapErr f status =
    case status of
        Failed x ->
            Failed (f x)

        Loading ->
            Loading

        Loaded a ->
            Loaded a


withDefault : a -> Status x a -> a
withDefault def status =
    case status of
        Loaded a ->
            a

        _ ->
            def


fromResult : Result err data -> Status err data
fromResult result =
    case result of
        Ok val ->
            Loaded val

        Err e ->
            Failed e
