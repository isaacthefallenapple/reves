module TypedDict exposing (TypedDict, from, fromListWithDefault, get, set, unwrap)


type TypedDict k v
    = TypedDict (List ( k, v ))


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
