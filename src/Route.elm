module Route exposing (Route(..), fromUrl, toMockUrl, toRootUrl)

import Url exposing (Url)
import Url.Builder as B
import Url.Parser exposing ((</>), Parser, int, map, oneOf, parse, s, string, top)


type Route
    = NotFound Url
    | Root
    | Mock


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Root top
        , map Root (s "root")
        , map Mock (s "mock")
        ]


fromUrl : Url -> Route
fromUrl url =
    parse routeParser url
        |> Maybe.withDefault (NotFound url)


toMockUrl : String
toMockUrl =
    B.absolute [ "mock" ] []


toRootUrl : String
toRootUrl =
    B.absolute [ "" ] []
