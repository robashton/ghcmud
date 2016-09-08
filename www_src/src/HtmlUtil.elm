module HtmlUtil exposing (onKeyPress)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode exposing (map)


onKeyPress : (Int -> msg) -> Attribute msg
onKeyPress tagger =
    on "keypress" (map tagger keyCode)
