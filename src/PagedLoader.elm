module PagedLoader exposing (..)

import ApiUrls
import Http
import Json.Decode as JD
import Json.Encode exposing (Value)


type alias PagedLoader a =
    { a
        | pagesFetched : Int
        , totalPages : Int
    }


pageLimit =
    20


type alias HttpResult a =
    Result Http.Error a


fetchNextPage : (HttpResult Value -> msg) -> PagedLoader a -> Cmd msg
fetchNextPage tagger model =
    if model.totalPages == model.pagesFetched then
        Cmd.none

    else
        Http.get
            { url = ApiUrls.getVideosPaged (model.pagesFetched + 1) pageLimit
            , expect = Http.expectJson tagger JD.value
            }
