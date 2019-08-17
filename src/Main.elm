module Main exposing (main)

import BasicsExtra exposing (callWith)
import Browser
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
import Html.Styled.Attributes as A exposing (class, css, height, href, src, width)
import Html.Styled.Events exposing (onClick)
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
            ( { model | playingVideo = Just video }, Ports.play video )


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
            viewMock model


viewMock model =
    { title = "Video Grid Layout Mock"
    , body =
        [ HasErrors.detailView model
        , viewMockGrid model
        ]
    }


imageUrl num =
    "img" ++ String.fromInt num ++ ".jpg"


mockSyn =
    "Your daily dose of news and gossip is back with Tea And T.V. The Kapil Sharma Show will soon have Vidya Balan</a> and Arjun Rampal as special guests and as usual Kapil didn&#39;t miss a chance to take a dig at Vidya and her obsession with the word &#39;Kahani&#39;, he even suggested Arjun to join politics and he gave a hilarious justification for it.<br /><br />Bigg Boss 10 house has a new captain and she&#39;s none other than VJ Bani, as soon as she became the captain, punishments and arguments followed. Swami Omji is having fun with Monalisa, he&#39;s playing with her in swimming pool and guess what Manoj is getting very jealous. Karan Patel has found the right balance between reel life and real, he recently shared a picture which clearly shows his happiness.<br /><br />Mouni Roy will soon be seen in a hot item song in the film Tum Bin 2 and she has started promoting it well, she has challenged few of her close television friends to show their dancing talent.<br /><br />Comedy Nights Bachao 2 is again in news for all the wrong reasons, after Amruta and Mannan, this season&#39;s host Mona Singh has also been replaced by none other than Bharti. Jhalak Dikhhla Jaa 9 will soon have a children&#39;s day special episode and check out the cuteness and energy of celebs. Jhalak&#39;s favourite Shantanu Maheshwari joined MyTV exclusively to share his childhood memories"


mockSynShort =
    "Your daily dose of news and gossip is back with Tea And T.V. The Kapil Sharma Show will soon have Vidya Balan</a> and Arjun Rampal as special guests and as usual Kapil didn&#39;t miss a chance to take a dig at Vidya and her obsession with the word &#39;Kahani&#39;, he even suggested Arjun to join politics and he gave a hilarious justification for it.<br /><br />Bigg Boss 10 house has a new captain and she&#39;s none other than VJ Bani, as soon as she became the captain, punishments and arguments followed. Swami Omji is having fun with Monalisa, he&#39;s playing with her in swimming pool and guess what Manoj is getting very jealous. Karan Patel has found the right balance between reel life and real, he recently shared a picture which clearly shows his happiness.<br /><br />Mouni Roy will soon be seen in a hot item song in the film Tum Bin 2 and she has started promoting it well, she has challenged few of her close television friends to show their dancing talent.<br /><br />Comedy Nights "


viewMockGrid _ =
    let
        viewImage idxFromOne =
            div [ css [ lineHeight zero, fontSize (pct 100) ] ] [ img [ src <| imageUrl idxFromOne ] [] ]
    in
    div [ class "vs3" ]
        [ div [ class "flex justify-around" ] [ viewImage 1, viewImage 2 ]
        , div [ class "flex justify-around" ]
            [ div [ class "flex items-center justify-center", css [ Css.minWidth (px 377) ] ]
                [ img
                    [ src <| imageUrl 4
                    , class ""
                    ]
                    []
                ]
            , div [ class "measure", css [] ]
                [ div [ class "f4 lh-title" ] [ text "Synopsis" ]
                , div
                    [ class "f6 lh-copy"
                    , css
                        [{- maxHeight (rem 5)
                            , overflow hidden
                            , textOverflow ellipsis
                         -}
                        ]
                    ]
                    [ text <| String.fromList <| List.take 300 <| String.toList mockSynShort
                    , text "..."
                    ]
                ]
            ]
        , div [ class "flex justify-around" ] [ viewImage 3, viewImage 4 ]
        , div [ class "flex justify-around" ] [ viewImage 2, viewImage 1 ]
        ]


viewHome : Model -> StyledDocument Msg
viewHome model =
    { title = "Movie Trailers"
    , body =
        [ HasErrors.detailView model
        , viewGallery model
        , div [ class "pre code" ] [ text model.dataStr ]
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
    div [ class "flex flex-column items-center" ]
        (groupedVideos |> List.concatMap (viewRow model))


playingVideoInList model videos =
    model.playingVideo
        |> Maybe.andThen
            (\v ->
                if List.member v videos then
                    Just v

                else
                    Nothing
            )


viewRow : Model -> List Video -> List (Html Msg)
viewRow model videos =
    let
        playingRow =
            playingVideoInList model videos
                |> Maybe.Extra.unwrap [] viewPlayingRow
    in
    playingRow ++ [ div [ class "flex " ] (List.map viewCell videos) ]


viewPlayingRow video =
    [ div [ class "flex" ]
        [ div [ class "w-60 relative" ]
            [ div [ A.id video.id ] []

            --            , div [ class "absolute absolute--fill bg-white-80 z-1" ] [ text "HWE" ]
            ]
        , div
            [ class "w-30"
            , css [ Css.height <| px 200 ]
            ]
            [ p [] [ text mockSynShort ]
            ]
        ]
    ]


viewCell vid =
    div
        [ class "tc flex-grow-1 flex-shrink-1"
        , css
            [ flexBasis (px 0)
            , maxWidth <| px 300
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
                ]
                []
            ]
        , div []
            [ div [ class "tc ph2" ] [ text vid.title ]
            ]
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
            (Html.Parser.Util.toVirtualDom
                >> List.map H.fromUnstyled
            )


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
