module Route exposing (Route(..), fromUrl, toDataUrl, toHomeUrl)

import Url exposing (Url)
import Url.Builder as B
import Url.Parser exposing ((</>), Parser, int, map, oneOf, parse, s, string, top)


type Route
    = NotFound Url
    | Home
    | Data


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top
        , map Data (s "data")
        ]


fromUrl : Url -> Route
fromUrl url =
    parse routeParser url
        |> Maybe.withDefault (NotFound url)


toDataUrl : String
toDataUrl =
    B.absolute [ "data" ] []


toHomeUrl : String
toHomeUrl =
    B.absolute [ "" ] []
