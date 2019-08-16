module Main exposing (main)

import BasicsExtra exposing (callWith)
import Browser
import Browser.Navigation as Nav
import Errors exposing (Errors)
import FontAwesome.Attributes
import FontAwesome.Icon as FAIcon
import FontAwesome.Styles
import HasErrors
import Html
import Html.Attributes as HA
import Html.Styled as H exposing (Html, div, source, text, video)
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



-- MODEL


type alias Model =
    { errors : Errors
    , key : Nav.Key
    , route : Route
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
            { errors = Errors.fromStrings [ "Testing Error View" ]
            , key = key
            , route = route
            }
    in
    model
        |> pure
        |> andThen (updateFromEncodedFlags encodedFlags)
        |> command fetchData


fetchData_ =
    Http.request
        { method = "GET"
        , url = "https://cors-anywhere.herokuapp.com/https://in.bookmyshow.com/serv/getData?cmd=GETVIDEOS&category=MYTV"
        , expect = Http.expectJson GotData JD.value
        , headers = []
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


fetchData =
    Http.get
        { url = "https://cors-anywhere.herokuapp.com/https://in.bookmyshow.com/serv/getData?cmd=GETVIDEOS&category=MYTV"
        , expect = Http.expectJson GotData JD.value
        }



-- MSG


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | GotData (Result Http.Error Value)



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
            ( { model | route = route }, {- queryTodoListForRouteCmd route -} Cmd.none )

        GotData res ->
            res
                |> Result.Extra.unpack httpError gotData
                |> callWith model


httpError e model =
    pure model


gotData d model =
    pure model


cacheEffect : Model -> Cmd msg
cacheEffect model =
    Ports.setCache (cacheEncoder (cacheFromModel model))


updateFromEncodedFlags : Value -> Model -> Return
updateFromEncodedFlags encodedFlags model =
    JD.decodeValue flagsDecoder encodedFlags
        |> Result.Extra.unpack onDecodeError updateFromFlags
        |> callWith model


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
        [ div []
            [ text "hw"
            ]
        , div [ class "pa3 dn" ] [ lazy lv 1 ]
        ]
    }


lv : a -> Html msg
lv _ =
    video
        [ A.id "vid1"
        , class "azuremediaplayer amp-default-skin"
        ]
        [ source
            [ src "//bmsmedia.streaming.mediaservices.windows.net/9e2d5489-82dd-42cc-8cbb-6726a9f34cfa/roommates agreement revised.ism/manifest(format=m3u8-aapl)"
            , type_ "application/vnd.ms-sstr+xml"
            ]
            []
        ]


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
