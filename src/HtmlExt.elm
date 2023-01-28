module HtmlExt exposing (..)

import Html exposing (Attribute)
import Json.Decode as Json
import Html.Events exposing (stopPropagationOn)

onClick : msg -> Attribute msg
onClick mapper =
    stopPropagationOn "input" (Json.succeed (mapper, True))
