module VideosResponse exposing (..)

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Video exposing (VideoDict)


type alias Page =
    { prev : Int
    , current : Int
    , next : Int
    , total : Int
    }


pageDecoder : Decoder Page
pageDecoder =
    let
        di name =
            JDP.required name JD.int
    in
    JD.succeed Page
        |> di "prev"
        |> di "current"
        |> di "next"
        |> di "total"


type alias VideosResponse =
    { page : Page
    , videoDict : VideoDict
    }


decoder : Decoder VideosResponse
decoder =
    JD.succeed VideosResponse
        |> JDP.requiredAt [ "MYTV", "page" ] pageDecoder
        |> JDP.requiredAt [ "MYTV", "CategoryVideoDetails" ] Video.dictDecoder
