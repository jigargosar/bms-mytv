module PagedLoader exposing
    ( Model
    , Msg(..)
    , fetchNextPage
    , init
    , update
    , updateFromVR
    )

import ApiUrls
import Http
import Json.Decode as JD
import Json.Encode exposing (Value)
import Maybe.Extra
import Task
import UpdateExtra exposing (pure)
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
                    let
                        cmd : msg
                        cmd =
                            config.onVideos (vr.videoList |> Video.sort)
                    in
                    if vr.page.current == 1 then
                        ( initLoadedStateFromVR vr
                        , Task.succeed identity |> Task.perform (always cmd)
                        )

                    else
                        pure model

                Loading loading ->
                    let
                        cmd : msg
                        cmd =
                            config.onVideos (vr.videoList |> Video.sort)
                    in
                    if vr.page.current == loading.pageNum then
                        ( initLoadedStateFromVR vr
                        , Task.succeed identity |> Task.perform (always cmd)
                        )

                    else
                        pure model

                LoadingThenFetchNext loading ->
                    let
                        cmd : msg
                        cmd =
                            config.onVideos (vr.videoList |> Video.sort)
                    in
                    if vr.page.current == loading.pageNum then
                        ( initLoadedStateFromVR vr
                        , Task.succeed identity |> Task.perform (always cmd)
                        )

                    else
                        pure model

                Loaded _ ->
                    pure model


fetchNextPage : (HttpResult Value -> msg) -> Model -> ( Model, Cmd msg )
fetchNextPage tagger model =
    case model of
        LoadingFirstPage ->
            ( model, Cmd.none )

        Loading pageNum ->
            ( LoadingThenFetchNext pageNum, Cmd.none )

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


updateFromVR : VideosResponse -> Model -> Maybe ( VideoList, Model )
updateFromVR vr model =
    case model of
        LoadingFirstPage ->
            if vr.page.current == 1 then
                Just ( vr.videoList |> Video.sort, initLoadedStateFromVR vr )

            else
                Nothing

        Loading loading ->
            updateFromVRIfPageNumEq loading.pageNum vr

        LoadingThenFetchNext loading ->
            updateFromVRIfPageNumEq loading.pageNum vr

        Loaded _ ->
            Nothing


updateFromVRIfPageNumEq pageNum vr =
    if vr.page.current == pageNum then
        ( vr.videoList |> Video.sort
        , initLoadedStateFromVR vr
        )
            |> Just

    else
        Nothing
