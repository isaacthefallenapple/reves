module Ability exposing (Ability, decoder, encode, view)

import Boon exposing (Boon)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias Ability =
    { name : String
    , flavor : Maybe String
    , boons : List Boon
    , text : String
    }



-- VIEW


view : Ability -> Html msg
view ability =
    details
        [ class "ability" ]
        [ summary
            []
            [ h3 [] [ text ability.name ] ]
        , p
            []
            ((ability.flavor
                |> Maybe.map (\flavor -> [ i [] [ text (flavor ++ " ") ] ])
                |> Maybe.withDefault []
             )
                ++ (if not (List.isEmpty ability.boons) then
                        [ text (String.join ". " (List.map Boon.toString ability.boons) ++ " ") ]

                    else
                        []
                   )
                ++ [ text ability.text
                   ]
            )
        ]



-- ENCODE


encode : Ability -> Encode.Value
encode ability =
    Encode.object
        [ ( "name", Encode.string ability.name )
        , ( "flavor"
          , ability.flavor
                |> Maybe.map Encode.string
                |> Maybe.withDefault Encode.null
          )
        , ( "boons", Encode.list Boon.encode ability.boons )
        , ( "text", Encode.string ability.text )
        ]



-- DECODER


decoder : Decoder Ability
decoder =
    Decode.map4 Ability
        (Decode.field "name" Decode.string)
        (Decode.field "flavor" (Decode.nullable Decode.string))
        (Decode.field "boons" (Decode.list Boon.decoder))
        (Decode.field "text" Decode.string)
