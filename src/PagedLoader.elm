module PagedLoader exposing
    ( Model
    , Msg(..)
    , getLoadingVideoCount
    , init
    , update
    )

import ApiUrls
import Http
import Json.Decode as JD
import Json.Encode exposing (Value)
import Task
import UpdateExtra exposing (andThen, pure)
import Video exposing (VideoList)
import VideosResponse exposing (VideosResponse)


type alias LoadingRecord =
    { pageNum : Int, totalPages : Int }


type alias LoadedRecord =
    { pagesLoaded : Int, totalPages : Int }


type Model
    = LoadingFirstPage
    | Loading LoadingRecord
    | LoadingThenFetchNext LoadingRecord
    | Loaded LoadedRecord


init : (HttpResult Value -> msg) -> ( Model, Cmd msg )
init tagger =
    let
        model =
            LoadingFirstPage
    in
    ( model, fetchPageNum tagger 1 )


getLoadingVideoCount : Model -> Int
getLoadingVideoCount model =
    case model of
        LoadingFirstPage ->
            pageLimit

        Loading _ ->
            pageLimit

        LoadingThenFetchNext _ ->
            pageLimit

        Loaded _ ->
            0


pageLimit =
    20


type alias HttpResult a =
    Result Http.Error a


type Msg
    = FetchNext
    | OnVideoResponse VideosResponse


type alias Config msg =
    { onHttpResult : HttpResult Value -> msg
    , onVideos : VideoList -> msg
    }


update : Config msg -> Msg -> Model -> ( Model, Cmd msg )
update config message model =
    case message of
        FetchNext ->
            fetchNextPage config.onHttpResult model

        OnVideoResponse vr ->
            case model of
                LoadingFirstPage ->
                    updateFromVRIfPageNumEq config 1 vr model
                        |> andThen (fetchNextPage config.onHttpResult)

                Loading loading ->
                    updateFromVRIfPageNumEq config loading.pageNum vr model

                LoadingThenFetchNext loading ->
                    updateFromVRIfPageNumEq config loading.pageNum vr model
                        |> andThen (fetchNextPage config.onHttpResult)

                Loaded _ ->
                    pure model


fetchNextPage : (HttpResult Value -> msg) -> Model -> ( Model, Cmd msg )
fetchNextPage tagger model =
    case model of
        LoadingFirstPage ->
            ( model, Cmd.none )

        Loading loading ->
            ( LoadingThenFetchNext loading, Cmd.none )

        LoadingThenFetchNext _ ->
            ( model, Cmd.none )

        Loaded loaded ->
            if loaded.pagesLoaded == loaded.totalPages then
                ( model, Cmd.none )

            else
                let
                    loadPageNum =
                        loaded.pagesLoaded + 1
                in
                ( Loading { pageNum = loadPageNum, totalPages = loaded.totalPages }
                , fetchPageNum tagger <| loadPageNum
                )


fetchPageNum tagger n =
    Http.get
        { url = ApiUrls.getVideosPaged n pageLimit
        , expect = Http.expectJson tagger JD.value
        }


initLoadedStateFromVR vr =
    Loaded <| LoadedRecord vr.page.current vr.page.total


updateFromVRIfPageNumEq :
    Config msg
    -> Int
    -> VideosResponse
    -> Model
    -> ( Model, Cmd msg )
updateFromVRIfPageNumEq config pageNum vr model =
    if vr.page.current == pageNum then
        ( initLoadedStateFromVR vr
        , msgToCmd <| config.onVideos (vr.videoList |> Video.sort)
        )

    else
        pure model


msgToCmd msg =
    Task.succeed identity |> Task.perform (always msg)
