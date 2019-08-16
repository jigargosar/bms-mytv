module Video exposing (..)

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP


type alias Video =
    { id : String
    , title : String
    , synopsis : String
    , videoUrl : String
    , imageUrl : String
    }


videoDecoder : Decoder Video
videoDecoder =
    let
        ds name =
            JDP.required name JD.string
    in
    JD.succeed Video
        |> ds "videoID"
        |> ds "title"
        |> ds "synopsis"
        |> ds "videoURL"
        |> ds "imageURL"
