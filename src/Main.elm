module Main exposing (main)

import BasicsExtra exposing (callWith)
import Browser
import Browser.Navigation as Nav
import Dict
import Errors exposing (Errors)
import FontAwesome.Attributes
import FontAwesome.Icon as FAIcon
import FontAwesome.Styles
import HasErrors
import Html
import Html.Attributes as HA
import Html.Parser
import Html.Parser.Util
import Html.Styled as H exposing (Html, div, h1, h3, img, source, text, video)
import Html.Styled.Attributes as A exposing (class, href, src, type_)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Lazy exposing (lazy)
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)
import Ports exposing (FirestoreQueryResponse)
import Result.Extra
import Return
import Route exposing (Route)
import UpdateExtra exposing (andThen, command, pure)
import Url exposing (Url)


type alias Video =
    { id : String
    , title : String
    , synopsis : String
    , videoUrl : String
    , imageUrl : String
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
        |> ds "synopsis"
        |> ds "videoURL"
        |> ds "imageURL"


videoListDecoder : Decoder (List Video)
videoListDecoder =
    JD.at [ "MYTV", "CategoryVideoDetails" ]
        (JD.dict videoDecoder
            |> JD.map Dict.values
        )



-- MODEL


type alias Model =
    { errors : Errors
    , key : Nav.Key
    , route : Route
    , dataStr : String
    , videos : List Video
    , playingVideo : Maybe Video
    }


type alias Cache =
    {}


type alias Flags =
    { cache : Cache
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
        { url = "https://cors-anywhere.herokuapp.com/https://in.bookmyshow.com/serv/getData?cmd=GETVIDEOS&category=MYTV&pageNumber=1&pageLimit=10"
        , expect = Http.expectJson GotData JD.value
        }



-- MSG


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | GotData (Result Http.Error Value)
    | PlayVideo Video



-- SUB


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []



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

        GotData res ->
            res
                |> Result.Extra.unpack httpError gotData
                |> callWith model

        PlayVideo video ->
            ( { model | playingVideo = Just video }, Cmd.none )


httpError e model =
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
            viewRoute Route.Root model

        Route.Root ->
            viewRoot model


viewRoot : Model -> StyledDocument Msg
viewRoot model =
    { title = "Movie Trailers"
    , body =
        [ HasErrors.detailView model
        , viewVideos model.videos
        , div [ class "pre code" ] [ text model.dataStr ]

        --        , div [ class "pa3 dn" ] [ lazy lv 1 ]
        ]
    }


viewVideos : List Video -> Html Msg
viewVideos videos =
    div []
        [ div [ class "f2 " ] [ text "Videos" ]
        , div [ class "vs3" ] (List.map viewVideo videos)
        ]


viewVideo : Video -> Html Msg
viewVideo video =
    div []
        [ div [ class "f3 pv1" ] [ text video.title ]
        , img [ src video.imageUrl, onClick (PlayVideo video) ] []
        , div [] (viewSynopsis video.synopsis)
        ]



--https://github.com/elm/html/issues/172#issuecomment-417891199


viewSynopsis : String -> List (Html msg)
viewSynopsis synopsis =
    Html.Parser.run synopsis
        |> Result.Extra.unpack (\_ -> [ text "" ])
            (Html.Parser.Util.toVirtualDom
                >> List.map H.fromUnstyled
            )



--lv : a -> Html msg
--lv _ =
--    video
--        [ A.id "vid1"
--        , class "azuremediaplayer amp-default-skin"
--        ]
--        [ source
--            [ src "//bmsmedia.streaming.mediaservices.windows.net/9e2d5489-82dd-42cc-8cbb-6726a9f34cfa/roommates agreement revised.ism/manifest(format=m3u8-aapl)"
--            , type_ "application/vnd.ms-sstr+xml"
--            ]
--            []
--        ]


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
