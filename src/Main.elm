module Main exposing (main)

import ApiUrls
import BasicsExtra exposing (callWith, eq_)
import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Css exposing (auto, flexBasis, fontSize, hex, int, lineHeight, maxWidth, none, num, outline, outline3, outlineStyle, outset, pct, pointerEvents, px, scale, transform, transforms, zIndex, zero)
import Css.Functional exposing (lh0)
import Dict
import Errors exposing (Errors)
import FontAwesome.Attributes
import FontAwesome.Icon as FAIcon
import FontAwesome.Styles
import HasErrors
import Html.Parser
import Html.Parser.Util
import Html.Styled as H exposing (Html, button, div, img, p, text, video)
import Html.Styled.Attributes as A exposing (class, css, height, href, src, style, width)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as K
import Html.Styled.Lazy exposing (lazy)
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)
import List.Extra
import Maybe.Extra
import Ports exposing (FirestoreQueryResponse)
import Result.Extra
import Return
import Route exposing (Route)
import Size exposing (Size)
import UpdateExtra exposing (andThen, command, effect, pure)
import Url exposing (Url)
import Video exposing (Video, VideoDict)
import VideosResponse exposing (VideosResponse)


videoListDecoder : Decoder (List Video)
videoListDecoder =
    JD.at [ "MYTV", "CategoryVideoDetails" ]
        (JD.dict Video.videoDecoder
            |> JD.map Dict.values
        )



-- MODEL


type alias Model =
    { errors : Errors
    , key : Nav.Key
    , size : Size
    , route : Route
    , dataStr : String
    , videos : List Video
    , pagesFetched : Int
    , totalPages : Int
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
        [ JD.succeed Cache
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


setPagesFetched : Int -> Model -> Model
setPagesFetched pagesFetched model =
    { model | pagesFetched = pagesFetched }



-- INIT


type alias Return =
    Return.Return Msg Model


init : Value -> Url -> Nav.Key -> Return
init encodedFlags url key =
    let
        route =
            Route.fromUrl url

        model : Model
        model =
            { errors = Errors.fromStrings []
            , key = key
            , size = Size.zero
            , route = route
            , dataStr = ""
            , videos = []
            , pagesFetched = 0
            , totalPages = -1
            , playingVideo = Nothing
            }
    in
    model
        |> pure
        |> andThen (decodeAndUpdate flagsDecoder updateFromFlags encodedFlags)
        |> effect fetchNextPage


pageLimit =
    10


fetchNextPage : Model -> Cmd Msg
fetchNextPage model =
    if model.totalPages == model.pagesFetched then
        Cmd.none

    else
        Http.get
            { url = ApiUrls.getVideosPaged (model.pagesFetched + 1) pageLimit
            , expect = Http.expectJson GotData JD.value
            }


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



-- SUB


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Size.onBrowserResize OnResize ]



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


httpError _ model =
    pure model


gotData : Value -> Model -> Return
gotData encodedData =
    formatAndSetEncodedData encodedData
        >> decodeAndUpdate VideosResponse.decoder handlePagedVideoResponse encodedData



--updateVideos : Value -> Model -> Return
--updateVideos encodedData =
--    decodeAndUpdate videoListDecoder
--        (\videos -> setVideos videos >> pure)
--        encodedData


handlePagedVideoResponse : VideosResponse -> Model -> Return
handlePagedVideoResponse vr model =
    model
        |> (if model.pagesFetched + 1 == vr.page.current then
                appendVideos (vr.videoList |> Video.sort)
                    >> setPagesFetched vr.page.current
                    >> pure
                    >> effect cacheEffect

            else
                pure
           )


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
        |> prependFontAwesomeCss


prependFontAwesomeCss : Browser.Document Msg -> Browser.Document Msg
prependFontAwesomeCss doc =
    { doc | body = FontAwesome.Styles.css :: doc.body }


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

        --        , div [ class "pre code" ] [ text model.dataStr ]
        ]
    }


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


viewGallery : Model -> Html Msg
viewGallery model =
    let
        thumbsPerRow =
            model.size.width // 250

        groupedVideos =
            getDisplayVideosList model
                |> List.Extra.groupsOf thumbsPerRow
    in
    div []
        [ div [ class "f2 " ] [ text "Videos" ]
        , viewRows model groupedVideos
        ]


viewRows : Model -> List (List Video) -> Html Msg
viewRows model groupedVideos =
    K.node "div"
        [ class "vs4 flex flex-column _items-center" ]
        (groupedVideos
            |> List.indexedMap (viewRow model)
            |> List.concat
        )


viewRow : Model -> Int -> List Video -> List ( String, Html Msg )
viewRow model rowIdx videos =
    let
        playingRow =
            playingVideoInList model videos
                |> Maybe.Extra.unwrap [] (viewPlayingRow model)
    in
    playingRow ++ [ ( String.fromInt rowIdx, div [ class "flex " ] (List.map (viewCell model) videos) ) ]


playingVideoInList model videos =
    model.playingVideo
        |> Maybe.andThen
            (\v ->
                if List.member v videos then
                    Just v

                else
                    Nothing
            )


videoContainerDomId videoId =
    videoId


viewPlayingRow : Model -> Video -> List ( String, Html Msg )
viewPlayingRow model video =
    let
        vidWidth =
            toFloat model.size.width
                * 60
                / 100
                |> Debug.log "vidWidth"

        vidHeight =
            9 / 16 * vidWidth
    in
    [ ( video.id
      , div
            [ class "flex w-100 relative "
            , css [ Css.marginBottom zero ]

            --            , style "background-color" "#3a434c"
            --            , style "background-color" "#282f35"
            --            , style "background-color" "#1c2125"
            , style "background-color" "#3C454F"
            ]
            [ div
                [ class "absolute absolute--fill  w-100 z-999"
                , style "box-shadow" "inset 0 0 8px 4px rgba(0,0,0,1)"
                , css [ pointerEvents none ]
                ]
                []
            , div [ class "w-60 _bg-black-30" ]
                [ div [ A.id <| videoContainerDomId video.id ] []
                ]
            , div
                [ class "w-40 pa3 pb0 flex flex-column"
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


viewCell model vid =
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

            --                    , class "pa1"
            ]
            [ img
                [ src vid.imageUrl
                , css []
                , class "w-100"
                ]
                []
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
                --                >> List.Extra.takeWhile (isBrTag >> not)
                >> Html.Parser.Util.toVirtualDom
                >> List.map H.fromUnstyled
            )


isBrTag node =
    case node of
        Html.Parser.Element "br" _ _ ->
            True

        _ ->
            False


removeTopLevelParagraphTag list =
    case list of
        [ Html.Parser.Element "p" _ nodes ] ->
            nodes

        _ ->
            list


faBtn : msg -> FAIcon.Icon -> Html msg
faBtn clickHandler icon =
    div
        [ class "gray hover-dark-gray pointer"
        , onClick clickHandler
        ]
        [ icon
            |> FAIcon.viewStyled [ FontAwesome.Attributes.lg ]
            |> H.fromUnstyled
        ]



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
