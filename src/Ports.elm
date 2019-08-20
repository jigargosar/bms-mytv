port module Ports exposing (..)

import Json.Encode exposing (Value)


port setCache : Value -> Cmd msg
