module TypedDict exposing (TypedDict, decoder, encode, from, fromListWithDefault, get, set, setAll, unwrap, update)

import Json.Decode as Decode
import Json.Encode as Encode


type TypedDict k v
    = TypedDict (List ( k, v ))


encode : (k -> String) -> (v -> Encode.Value) -> TypedDict k v -> Encode.Value
encode toString valueEncoder (TypedDict dict) =
    Encode.object (List.map (Tuple.mapBoth toString valueEncoder) dict)


decoder : (String -> k) -> Decode.Decoder v -> Decode.Decoder (TypedDict k v)
decoder fromString valueDecoder =
    Decode.map
        (List.map
            (Tuple.mapFirst fromString)
            >> from
        )
        (Decode.keyValuePairs valueDecoder)


from : List ( k, v ) -> TypedDict k v
from list =
    TypedDict list


unwrap : TypedDict k v -> List ( k, v )
unwrap (TypedDict list) =
    list


fromListWithDefault : v -> List k -> TypedDict k v
fromListWithDefault defaultVal keys =
    TypedDict (List.map (\k -> ( k, defaultVal )) keys)


get : k -> TypedDict k v -> Maybe v
get key (TypedDict dict) =
    dict
        |> List.filter (Tuple.first >> (==) key)
        |> List.head
        |> Maybe.map Tuple.second


set : k -> v -> TypedDict k v -> TypedDict k v
set key val (TypedDict dict) =
    List.map
        (\( k, v ) ->
            if k == key then
                ( k, val )

            else
                ( k, v )
        )
        dict
        |> TypedDict


setAll : List ( k, v ) -> TypedDict k v -> TypedDict k v
setAll kvPairs dict =
    List.foldl (\( k, v ) -> set k v) dict kvPairs


update : k -> (v -> v) -> TypedDict k v -> TypedDict k v
update key updater (TypedDict dict) =
    List.map
        (\( k, v ) ->
            if k == key then
                ( k, updater v )

            else
                ( k, v )
        )
        dict
        |> TypedDict
