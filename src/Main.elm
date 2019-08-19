module Main exposing (main)

--import FontAwesome.Styles

import BasicsExtra exposing (callWith, eq_)
import Browser
import Browser.Navigation as Nav
import Css exposing (flexBasis, none, pointerEvents, px)
import Css.Functional exposing (..)
import Errors exposing (Errors)
import HasErrors
import Html.Parser
import Html.Parser.Util
import Html.Styled as H exposing (Html, button, div, img, text, video)
import Html.Styled.Attributes as A exposing (class, css, height, href, src, style)
import Html.Styled.Events exposing (on, onClick)
import Html.Styled.Keyed as K
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)
import List.Extra
import Maybe.Extra
import PagedLoader
import Ports exposing (FirestoreQueryResponse)
import Result.Extra
import Return
import Route exposing (Route)
import Size exposing (Size)
import UpdateExtra exposing (andThen, command, effect, pure)
import Url exposing (Url)
import Video exposing (Video, VideoDict, VideoList)
import VideosResponse exposing (VideosResponse)



-- MODEL


type alias Model =
    { errors : Errors
    , key : Nav.Key
    , size : Size
    , route : Route
    , dataStr : String
    , videos : List Video
    , pagedLoader : PagedLoader.Model
    , playingVideo : Maybe Video
    }


type alias Cache =
    { videos : List Video
    }


type alias Flags =
    { cache : Cache
    , size : Size
    }


cacheDecoder : Decoder Cache
cacheDecoder =
    let
        initCache =
            { videos = [] }
    in
    JD.oneOf
        [ JD.succeed initCache
        , JD.succeed Cache
            |> JDP.optional "videos" Video.listDecoder []
        , JD.null initCache
        , JD.succeed initCache
        ]


cacheEncoder : Cache -> Value
cacheEncoder { videos } =
    JE.object
        [ ( "videos", Video.listEncoder videos ) ]


setModelFromCache : Cache -> Model -> Model
setModelFromCache { videos } model =
    { model | videos = videos }


cacheFromModel : Model -> Cache
cacheFromModel model =
    { videos = model.videos
    }


flagsDecoder : Decoder Flags
flagsDecoder =
    JD.succeed Flags
        |> JDP.required "cache" cacheDecoder
        |> JDP.required "size" Size.decoder


setSize : a -> { b | size : a } -> { b | size : a }
setSize size model =
    { model | size = size }


formatAndSetEncodedData : Value -> Model -> Model
formatAndSetEncodedData encoded model =
    { model | dataStr = JE.encode 2 encoded }


appendVideos : List Video -> Model -> Model
appendVideos videos model =
    { model | videos = model.videos ++ videos }


setPagedLoader : a -> { b | pagedLoader : a } -> { b | pagedLoader : a }
setPagedLoader pagedLoader model =
    { model | pagedLoader = pagedLoader }



-- INIT


type alias Return =
    Return.Return Msg Model


init : Value -> Url -> Nav.Key -> Return
init encodedFlags url key =
    let
        route =
            Route.fromUrl url

        ( pagedLoader, plCmd ) =
            PagedLoader.init GotData

        model : Model
        model =
            { errors = Errors.fromStrings []
            , key = key
            , size = Size.zero
            , route = route
            , dataStr = ""
            , videos = []
            , pagedLoader = pagedLoader
            , playingVideo = Nothing
            }
    in
    model
        |> pure
        |> andThen (decodeAndUpdate flagsDecoder updateFromFlags encodedFlags)
        |> command plCmd


fetchNextPage : Model -> Return
fetchNextPage =
    update (OnPageLoaderMsg PagedLoader.FetchNext)


type alias HttpResult a =
    Result Http.Error a



-- MSG


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | OnResize Size
    | GotData (HttpResult Value)
    | Play Video
    | Close
    | More
    | OnPageLoaderMsg PagedLoader.Msg
    | AppendVideos VideoList



-- SUB


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Size.onBrowserResize OnResize
        , Ports.more <| always More
        ]



-- UPDATE


update : Msg -> Model -> Return
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    if Route.fromUrl url == model.route then
                        ( model, Nav.replaceUrl model.key (Url.toString url) )

                    else
                        ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                route =
                    Route.fromUrl url
            in
            ( { model | route = route }, Cmd.none )

        OnResize size ->
            pure { model | size = size }

        GotData res ->
            res
                |> Result.Extra.unpack httpError gotData
                |> callWith model

        Play video ->
            ( { model | playingVideo = Just video }
            , Cmd.batch
                [ Ports.play video
                ]
            )

        Close ->
            ( { model | playingVideo = Nothing }
            , Cmd.batch
                [ Ports.disposePlayer ()
                ]
            )

        More ->
            fetchNextPage model

        OnPageLoaderMsg msg ->
            PagedLoader.update config msg model.pagedLoader
                |> Tuple.mapFirst (setPagedLoader >> callWith model)

        AppendVideos videos ->
            appendVideos videos model
                |> pure
                |> effect cacheEffect


config =
    { onHttpResult = GotData
    , onVideos = AppendVideos
    }


httpError _ model =
    pure model


gotData : Value -> Model -> Return
gotData encodedData =
    formatAndSetEncodedData encodedData
        >> decodeAndUpdate VideosResponse.decoder handlePagedVideoResponse encodedData


updatePageLoader message =
    update (OnPageLoaderMsg message)


handlePagedVideoResponse : VideosResponse -> Model -> Return
handlePagedVideoResponse vr =
    updatePageLoader (PagedLoader.OnVideoResponse vr)


decodeAndUpdate : Decoder a -> (a -> Model -> Return) -> Value -> Model -> Return
decodeAndUpdate decoder onSuccess encoded model =
    JD.decodeValue decoder encoded
        |> Result.Extra.unpack onDecodeError onSuccess
        |> callWith model


cacheEffect : Model -> Cmd msg
cacheEffect model =
    Ports.setCache (cacheEncoder (cacheFromModel model))


updateFromFlags : Flags -> Model -> Return
updateFromFlags flags model =
    model
        |> setSize flags.size
        |> setModelFromCache flags.cache
        |> pure


onDecodeError : JD.Error -> Model -> Return
onDecodeError error model =
    HasErrors.prependDecodeError error model
        |> pure



-- VIEW


view : Model -> Browser.Document Msg
view model =
    viewRoute model.route model
        |> toUnStyledDocument



--        |> prependFontAwesomeCss
--prependFontAwesomeCss : Browser.Document Msg -> Browser.Document Msg
--prependFontAwesomeCss doc =
--    { doc | body = FontAwesome.Styles.css :: doc.body }


type alias StyledDocument msg =
    { title : String, body : List (Html msg) }


toUnStyledDocument : StyledDocument msg -> Browser.Document msg
toUnStyledDocument { title, body } =
    { title = title, body = body |> List.map H.toUnstyled }


viewRoute : Route -> Model -> StyledDocument Msg
viewRoute route model =
    case route of
        Route.NotFound _ ->
            viewRoute Route.Home model

        Route.Home ->
            viewHome model

        Route.Data ->
            viewData model


viewHome : Model -> StyledDocument Msg
viewHome model =
    { title = "Movie Trailers"
    , body =
        [ HasErrors.detailView model
        , viewGallery model
        , viewFooter model

        --        , div [ class "pre code" ] [ text model.dataStr ]
        ]
    }


viewFooter _ =
    div [ css [ Css.height <| px 400 ] ] []


viewData : Model -> StyledDocument Msg
viewData model =
    { title = "Data"
    , body =
        [ HasErrors.detailView model
        , div [ class "pre code" ] [ text model.dataStr ]
        ]
    }


getDisplayVideosList model =
    model.videos


thumbsPerRow : Model -> Int
thumbsPerRow model =
    model.size.width // 250


thumbAspectRatio : Float
thumbAspectRatio =
    -- 345 / 184
    -- 15 / 8
    16 / 9


thumbHeight : Model -> Float
thumbHeight model =
    let
        cellWidth =
            toFloat model.size.width / (toFloat <| thumbsPerRow model)
    in
    1 / thumbAspectRatio * cellWidth


type Cell
    = ImageCell Video
    | LoadingCell


viewGallery : Model -> Html Msg
viewGallery model =
    let
        displayVideos =
            getDisplayVideosList model

        displayVideosCount =
            displayVideos |> List.length

        loadingVideoCount =
            model.pagedLoader
                |> PagedLoader.getLoadingVideoCount

        loadingCells =
            List.repeat loadingVideoCount LoadingCell

        cells =
            displayVideos
                |> List.map ImageCell
                |> (\l -> l ++ loadingCells)

        rowCellCount =
            thumbsPerRow model

        --
        --
        --        remainder =
        --            modBy rowCellCount displayVideosCount
        --                |> Debug.log "rowCount"
        groupedVideos =
            cells
                |> List.Extra.greedyGroupsOf rowCellCount
    in
    div []
        [ div [ class "ph3 pv1 f3 lh-copy" ] [ text "MyTV" ]
        , viewRows model groupedVideos
        , H.node "load-more" [ on "intersectionChanged" (JD.succeed More) ] []
        ]


viewRows : Model -> List (List Cell) -> Html Msg
viewRows model groupedVideos =
    K.node "div"
        [ class "vs4 flex flex-column _items-center" ]
        (groupedVideos
            |> List.indexedMap (viewRow model)
            |> List.concat
        )


viewRow : Model -> Int -> List Cell -> List ( String, Html Msg )
viewRow model rowIdx videos =
    let
        playingRow =
            playingVideoInList model videos
                |> Maybe.Extra.unwrap [] (viewPlayingRow model)

        videoCount =
            List.length videos

        fillerCellCount =
            thumbsPerRow model - videoCount

        fillerCells =
            List.repeat fillerCellCount viewFillerCell
    in
    playingRow
        ++ [ ( String.fromInt rowIdx
             , div [ class "flex " ]
                (List.map (viewCell model) videos ++ fillerCells)
             )
           ]


playingVideoInList model videos =
    model.playingVideo
        |> Maybe.andThen
            (\v ->
                if List.member (ImageCell v) videos then
                    Just v

                else
                    Nothing
            )


videoContainerDomId videoId =
    videoId


computeVideoPlayerHeight model =
    let
        vidWidth =
            toFloat model.size.width
                * 70
                / 100

        --                |> Debug.log "vidWidth"
        vidHeight =
            9 / 16 * vidWidth
    in
    vidHeight


viewPlayingRow : Model -> Video -> List ( String, Html Msg )
viewPlayingRow model video =
    let
        vidHeight =
            computeVideoPlayerHeight model
    in
    [ ( video.id
      , div
            [ class "flex w-100 relative"
            , css [ mb0, bgHex "#3C454F" ]
            ]
            [ div
                [ class "absolute absolute--fill  w-100 z-999"
                , style "box-shadow" "inset 0 0 8px 4px rgba(0,0,0,1)"
                , css [ pointerEvents none ]
                ]
                []
            , div [ class "w-70 _bg-black-30" ]
                [ div [ A.id <| videoContainerDomId video.id ] []
                ]
            , div
                [ class "w-30 pa3 pb0 flex flex-column"
                , css [ Css.height <| px (vidHeight - 6) ]
                ]
                [ div [ class "f4 lh-title" ] [ text video.title ]
                , button [ onClick Close ] [ text "close" ]
                , div [ class "flex" ] [ text video.subCategory ]
                , div [ class "f7 overflow-hidden lh-copy " ]
                    [ div [] (viewSynopsis video.synopsis)
                    ]
                ]
            ]
      )
    ]


viewFillerCell =
    div
        [ class "flex-grow-1 flex-shrink-1"
        , css [ flexBasis (px 0) ]
        ]
        []


viewCell model vid =
    case vid of
        ImageCell video ->
            viewImageCell model video

        LoadingCell ->
            let
                cellHeight =
                    thumbHeight model + 50
            in
            div
                [ class "flex-grow-1 flex-shrink-1 bg-pink"
                , css [ flexBasis (px 0), Css.height <| px cellHeight ]
                ]
                []


viewImageCell model vid =
    let
        isSel =
            model.playingVideo
                |> Maybe.Extra.unwrap False (eq_ vid)
    in
    div
        [ class "flex-grow-1 flex-shrink-1 relative"
        , style "box-shadow"
            (if isSel then
                "inset 0 0 1px 1px white"

             else
                "none"
            )
        , css
            [ flexBasis (px 0)
            , Css.batch
                (if isSel then
                    []

                 else
                    []
                )

            --            , maxWidth <| px 250
            ]

        --                , class "flex flex-column"
        , onClick <| Play vid
        ]
        [ div
            [ css []
            , class "flex items-center justify-center"
            , class "pa3"
            ]
            [ H.node "lazy-image"
                []
                [ img
                    [ A.attribute "data-src" vid.imageUrl
                    , height (thumbHeight model |> round)
                    ]
                    []
                ]
            ]
        , div
            [ class "pa2 f6 lh-title "
            , style "text-shadow" "1px 1px 2px black"
            ]
            [ text vid.title ]
        ]


{-| Prevent XSS since synopsis contains HTML

  - Why does elm does this?
    A: Excellent explanation from Creator of Elm
    <https://github.com/elm/html/issues/172#issuecomment-417891199>

-}
viewSynopsis : String -> List (Html Msg)
viewSynopsis synopsis =
    Html.Parser.run synopsis
        |> Result.Extra.unpack (\_ -> [ text "" ])
            (removeTopLevelParagraphTag
                >> Html.Parser.Util.toVirtualDom
                >> List.map H.fromUnstyled
            )


removeTopLevelParagraphTag list =
    case list of
        [ Html.Parser.Element "p" _ nodes ] ->
            nodes

        _ ->
            list



-- MAIN


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
