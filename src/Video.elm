module Video exposing (..)

import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)


type alias Video =
    { id : String
    , title : String
    , subCategory : String
    , liveDate : String
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
        |> ds "liveDate"
        |> JDP.required "videoTags" (JD.list JD.string)
        |> ds "synopsis"
        |> ds "videoURL"
        |> ds "imageURL"
        |> ds "views"
        |> JDP.required "likes" JD.int


videoEncoder : Video -> Value
videoEncoder { id, title, subCategory, liveDate, tags, synopsis, videoUrl, imageUrl, views, likes } =
    JE.object
        [ ( "videoID", JE.string id )
        , ( "title", JE.string title )
        , ( "subCategory", JE.string subCategory )
        , ( "liveDate", JE.string liveDate )
        , ( "videoTags", JE.list JE.string tags )
        , ( "synopsis", JE.string synopsis )
        , ( "videoURL", JE.string videoUrl )
        , ( "imageURL", JE.string imageUrl )
        , ( "views", JE.string views )
        , ( "likes", JE.int likes )
        ]


type alias VideoList =
    List Video


listDecoder : Decoder (List Video)
listDecoder =
    JD.list videoDecoder


listEncoder : List Video -> Value
listEncoder =
    JE.list videoEncoder


type alias VideoDict =
    Dict String Video


dictDecoder =
    JD.dict videoDecoder


sort =
    List.sortBy .liveDate >> List.reverse
