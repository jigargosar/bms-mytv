module Api exposing (..)

import Http
import Json.Decode as JD


fetchData tagger =
    Http.get
        { url = "https://cors-anywhere.herokuapp.com/https://in.bookmyshow.com/serv/getData?cmd=GETVIDEOS&category=MYTV&pageNumber=1&pageLimit=30"
        , expect = Http.expectJson tagger JD.value
        }
