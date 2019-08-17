module Main exposing (main)

import BasicsExtra exposing (callWith)
import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Css exposing (flexBasis, fontSize, int, lineHeight, maxWidth, num, pct, px, zero)
import Dict
import Errors exposing (Errors)
import FontAwesome.Attributes
import FontAwesome.Icon as FAIcon
import FontAwesome.Styles
import HasErrors
import Html.Parser
import Html.Parser.Util
import Html.Styled as H exposing (Html, div, img, p, text, video)
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
import UpdateExtra exposing (andThen, command, pure)
import Url exposing (Url)
import Video exposing (Video)


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
    , playingVideo : Maybe Video
    }


type alias Cache =
    {}


type alias Flags =
    { cache : Cache
    , size : Size
    }


cacheDecoder : Decoder Cache
cacheDecoder =
    JD.succeed Cache


cacheEncoder : Cache -> Value
cacheEncoder _ =
    JE.object
        []


setModelFromCache : Cache -> Model -> Model
setModelFromCache _ model =
    model


cacheFromModel : Model -> Cache
cacheFromModel _ =
    {}


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


setVideos : List Video -> Model -> Model
setVideos videos model =
    { model | videos = videos }



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
            , playingVideo = Nothing
            }
    in
    model
        |> pure
        |> andThen (decodeAndUpdate flagsDecoder updateFromFlags encodedFlags)
        |> command fetchData


fetchData =
    Http.get
        { url = "https://cors-anywhere.herokuapp.com/https://in.bookmyshow.com/serv/getData?cmd=GETVIDEOS&category=MYTV&pageNumber=1&pageLimit=30"
        , expect = Http.expectJson GotData JD.value
        }



-- MSG


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | OnResize Size
    | GotData (Result Http.Error Value)
    | Play Video



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


httpError _ model =
    pure model


gotData : Value -> Model -> Return
gotData encodedData model =
    formatAndSetEncodedData encodedData model
        |> decodeAndUpdate videoListDecoder
            (\videos -> setVideos videos >> pure)
            encodedData


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

        Route.Mock ->
            viewRoute Route.Home model


viewHome : Model -> StyledDocument Msg
viewHome model =
    { title = "Movie Trailers"
    , body =
        [ HasErrors.detailView model
        , viewGallery model

        --        , div [ class "pre code" ] [ text model.dataStr ]
        ]
    }


viewGallery : Model -> Html Msg
viewGallery model =
    let
        thumbsPerRow =
            model.size.width // 250

        groupedVideos =
            model.videos
                |> List.Extra.groupsOf thumbsPerRow
    in
    div []
        [ div [ class "f2 " ] [ text "Videos" ]
        , viewRows model groupedVideos
        ]


viewRows : Model -> List (List Video) -> Html Msg
viewRows model groupedVideos =
    K.node "div"
        [ class "vs3 flex flex-column _items-center" ]
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
    playingRow ++ [ ( String.fromInt rowIdx, div [ class "flex " ] (List.map viewCell videos) ) ]


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
      , div [ class "flex w-100 bg-black-20" ]
            [ div [ class "w-60 relative bg-black-10" ]
                [ div [ A.id <| videoContainerDomId video.id ] []
                ]
            , div [ class "w-40 ph4 " ]
                [ div
                    [ class "vs3 flex flex-column"
                    , css [ Css.height <| px (vidHeight - 6) ]
                    ]
                    [ div [ class "f4 lh-title" ] [ text video.title ]
                    , div [ class "f7 overflow-hidden lh-copy" ]
                        [ div [] (viewSynopsis video.synopsis)
                        ]
                    ]
                ]
            ]
      )
    ]


viewCell vid =
    div
        [ class "flex-grow-1 flex-shrink-1 relative"
        , css
            [ flexBasis (px 0)

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
