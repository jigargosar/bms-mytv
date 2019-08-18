module PagedLoader exposing
    ( PagedLoader
    , fetchNextPage
    , init
    , updateFromVR
    )

import ApiUrls
import Http
import Json.Decode as JD
import Json.Encode exposing (Value)
import Video exposing (VideoList)
import VideosResponse exposing (VideosResponse)


type alias PagedLoader =
    { pagesFetched : Int
    , totalPages : Int
    }


init : PagedLoader
init =
    { pagesFetched = -1, totalPages = 0 }


pageLimit =
    20


type alias HttpResult a =
    Result Http.Error a


fetchNextPage : (HttpResult Value -> msg) -> PagedLoader -> Cmd msg
fetchNextPage tagger model =
    if model.totalPages == model.pagesFetched then
        Cmd.none

    else
        Http.get
            { url = ApiUrls.getVideosPaged (model.pagesFetched + 1) pageLimit
            , expect = Http.expectJson tagger JD.value
            }


setPagesFetched : a -> { b | pagesFetched : a } -> { b | pagesFetched : a }
setPagesFetched pagesFetched model =
    { model | pagesFetched = pagesFetched }


updateFromVR : VideosResponse -> PagedLoader -> Maybe ( VideoList, PagedLoader )
updateFromVR vr model =
    if model.pagesFetched + 1 == vr.page.current then
        ( vr.videoList |> Video.sort, setPagesFetched vr.page.current model )
            |> Just

    else
        Nothing
