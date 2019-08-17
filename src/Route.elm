module Route exposing (Route(..), fromUrl, toHomeUrl, toMockUrl)

import Url exposing (Url)
import Url.Builder as B
import Url.Parser exposing ((</>), Parser, int, map, oneOf, parse, s, string, top)


type Route
    = NotFound Url
    | Home
    | Mock


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top
        , map Mock (s "mock")
        ]


fromUrl : Url -> Route
fromUrl url =
    parse routeParser url
        |> Maybe.withDefault (NotFound url)


toMockUrl : String
toMockUrl =
    B.absolute [ "mock" ] []


toHomeUrl : String
toHomeUrl =
    B.absolute [ "" ] []
