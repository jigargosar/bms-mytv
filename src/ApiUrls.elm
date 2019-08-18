module ApiUrls exposing (getVideosPaged)

import Url.Builder as Builder exposing (QueryParameter, int, string)


corsUrl =
    "https://cors-anywhere.herokuapp.com"


getDataUrl =
    "https://in.bookmyshow.com/serv/getData"


getVideosParams : List QueryParameter
getVideosParams =
    [ string "cmd" "GETVIDEOS", string "category" "MYTV" ]


pageNumber : Int -> QueryParameter
pageNumber n =
    int "pageNumber" n


pageLimit : Int -> QueryParameter
pageLimit s =
    int "pageLimit" s


getVideosPaged : Int -> Int -> String
getVideosPaged pn pl =
    Builder.crossOrigin corsUrl
        [ getDataUrl ]
        (getVideosParams ++ [ pageNumber pn, pageLimit pl ])
