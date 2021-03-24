module Route exposing (..)

import PlayAids
import Url
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), Parser, fragment, map, oneOf, s)


type Route
    = Root
    | Abilities (Maybe String)
    | PlayAid PlayAids.Topic (Maybe String)


parser : Parser (Route -> a) a
parser =
    s "reves"
        </> oneOf
                [ map Root Parser.top
                , map Abilities (s "abilities" </> fragment identity)
                , map PlayAid (s "play-aid" </> playAid </> fragment identity)
                ]


playAid : Parser (PlayAids.Topic -> a) a
playAid =
    Parser.custom "TOPIC" PlayAids.topicFromString


parse : Url.Url -> Route
parse url =
    Parser.parse parser url
        |> Maybe.withDefault Root


toString : Route -> String
toString route =
    let
        absolute path query frag =
            Builder.custom Builder.Absolute ("reves" :: path) query frag
    in
    case route of
        Root ->
            absolute [] [] Nothing

        Abilities selected ->
            absolute [ "abilities" ] [] selected

        PlayAid topic selected ->
            absolute [ "play-aid", PlayAids.topicToString topic ] [] selected
