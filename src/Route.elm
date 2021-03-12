module Route exposing (..)

import Url
import Url.Parser as Parser exposing ((</>), Parser, fragment, map, oneOf, s)


type Route
    = Root
    | Abilities (Maybe String)


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Root Parser.top
        , map Abilities (s "abilities" </> fragment identity)
        ]


parse : Url.Url -> Route
parse url =
    Parser.parse parser url
        |> Maybe.withDefault Root


toString : Route -> String
toString route =
    "/"
        ++ (case route of
                Root ->
                    ""

                Abilities (Just frag) ->
                    "abilities#" ++ frag

                Abilities Nothing ->
                    "abilities"
           )
