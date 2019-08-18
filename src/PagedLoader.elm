module PagedLoader exposing
    ( Model
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
    | Loading Int
    | LoadingThenFetchNext Int
    | Loaded Int Int


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


fetchNextPage : (HttpResult Value -> msg) -> Model -> ( Model, Cmd msg )
fetchNextPage tagger model =
    case model of
        LoadingFirstPage ->
            ( model, Cmd.none )

        Loading pageNum ->
            ( LoadingThenFetchNext pageNum, Cmd.none )

        LoadingThenFetchNext _ ->
            ( model, Cmd.none )

        Loaded pagesFetched totalPages ->
            if pagesFetched == totalPages then
                ( model, Cmd.none )

            else
                let
                    loadPageNum =
                        pagesFetched + 1
                in
                ( Loading loadPageNum, fetchPageNum tagger <| pagesFetched + 1 )


fetchPageNum tagger n =
    Http.get
        { url = ApiUrls.getVideosPaged n pageLimit
        , expect = Http.expectJson tagger JD.value
        }


updateFromVR : VideosResponse -> Model -> Maybe ( VideoList, Model )
updateFromVR vr model =
    case model of
        LoadingFirstPage ->
            if vr.page.current == 1 then
                Just ( vr.videoList |> Video.sort, Loaded 1 vr.page.total )

            else
                Nothing

        Loading pageNum ->
            updateFromVRIfPageNumEq pageNum vr

        LoadingThenFetchNext pageNum ->
            updateFromVRIfPageNumEq pageNum vr

        Loaded _ _ ->
            Nothing


updateFromVRIfPageNumEq pageNum vr =
    if vr.page.current == pageNum then
        ( vr.videoList |> Video.sort
        , Loaded vr.page.current vr.page.total
        )
            |> Just

    else
        Nothing
