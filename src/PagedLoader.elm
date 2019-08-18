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


type Model
    = LoadingFirstPage
    | Loaded Int Int
    | Loading Int Int


type alias PagedLoader =
    --    { pagesFetched : Int
    --    , totalPages : Int
    --    }
    Model


init : (HttpResult Value -> msg) -> ( PagedLoader, Cmd msg )
init tagger =
    let
        model =
            LoadingFirstPage
    in
    ( model, fetchNextPage tagger model )


pageLimit =
    20


type alias HttpResult a =
    Result Http.Error a


fetchNextPage : (HttpResult Value -> msg) -> PagedLoader -> Cmd msg
fetchNextPage tagger model =
    case model of
        LoadingFirstPage ->
            Cmd.none

        Loading _ _ ->
            Cmd.none

        Loaded pagesFetched totalPages ->
            if pagesFetched == totalPages then
                Cmd.none

            else
                Http.get
                    { url = ApiUrls.getVideosPaged (pagesFetched + 1) pageLimit
                    , expect = Http.expectJson tagger JD.value
                    }


updateFromVR : VideosResponse -> PagedLoader -> Maybe ( VideoList, PagedLoader )
updateFromVR vr model =
    case model of
        LoadingFirstPage ->
            if vr.page.current == 1 then
                Just ( vr.videoList |> Video.sort, Loaded 1 vr.page.total )

            else
                Nothing

        Loading pagesFetched _ ->
            if vr.page.current == pagesFetched + 1 then
                Just ( vr.videoList |> Video.sort, Loaded vr.page.current vr.page.total )

            else
                Nothing

        Loaded _ _ ->
            Nothing
