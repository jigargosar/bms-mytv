module Video exposing (..)

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP


type alias Video =
    { id : String
    , title : String
    , subCategory : String
    , tags : List String
    , synopsis : String
    , videoUrl : String
    , imageUrl : String
    , views : String
    , likes : Int
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
        |> ds "subCategory"
        |> JDP.required "videoTags" (JD.list JD.string)
        |> ds "synopsis"
        |> ds "videoURL"
        |> ds "imageURL"
        |> ds "views"
        |> JDP.required "likes" JD.int
