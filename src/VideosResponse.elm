module VideosResponse exposing (..)

import Dict
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Video exposing (VideoDict, VideoList)


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
    , videoList : VideoList
    }


decoder : Decoder VideosResponse
decoder =
    JD.succeed VideosResponse
        |> JDP.requiredAt [ "MYTV", "page" ] pageDecoder
        |> JDP.requiredAt [ "MYTV", "CategoryVideoDetails" ]
            (Video.dictDecoder |> JD.map Dict.values)
