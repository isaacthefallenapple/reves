module Route exposing (..)

import Url
import Url.Parser exposing ((</>), Parser, fragment, map, oneOf, s)


type Route
    = Root
    | Abilities (Maybe String)


route : Parser (Route -> a) a
route =
    oneOf
        [ map Root (s "")
        , map Abilities (s "abilities" </> fragment identity)
        ]


parse : Url.Url -> Maybe Route
parse =
    Url.Parser.parse route


toString : Maybe Route -> String
toString maybeRoute =
    maybeRoute
        |> Maybe.map
            (\r ->
                case r of
                    Root ->
                        ""

                    Abilities (Just frag) ->
                        "abilities#" ++ frag

                    Abilities Nothing ->
                        "abilities"
            )
        |> Maybe.withDefault ""
