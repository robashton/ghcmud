module Bootstrap exposing (..)

import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html exposing (..)


container : List (Html msg) -> Html msg
container children =
    div [ class "container-fluid" ] children


row : List (Html msg) -> Html msg
row children =
    div [ class "row" ] children


col : Int -> List (Html msg) -> Html msg
col span children =
    div [ class ("col-md-" ++ (toString span)) ] children


dl : List (Html msg) -> Html msg
dl children =
    Html.dl [ class "dl-horizontal" ] children


primaryButton : msg -> String -> Html msg
primaryButton handler message =
    Html.button [ class "btn btn-primary", onClick handler ] [ text message ]


table : List (Html msg) -> Html msg
table children =
    div [ class "table-responsive" ]
        [ Html.table [ class "table table-striped" ] children
        ]
