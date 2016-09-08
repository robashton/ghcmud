module Main exposing (..)

import Html.App as App
import Task
import Http
import Api
import Bootstrap
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import HtmlUtil exposing (..)


type alias Model =
    { username : Maybe String
    , history : List String
    , command : String
    }


type Msg
    = FetchFail Http.Error
    | CommandChanged String
    | CommandResult String
    | LoginRequested
    | MaybeLoginReturn Int
    | CommandSubmitted
    | LoginSucceeded String
    | MaybeCommandReturn Int


init : ( Model, Cmd Msg )
init =
    { username = Nothing
    , history = []
    , command = ""
    }
        ! [ Cmd.none ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchFail err ->
            ( { model | history = ((toString err) :: model.history) }, Cmd.none )

        CommandResult str ->
            ( { model | history = (str :: model.history) }, Cmd.none )

        CommandChanged str ->
            ( { model | command = str }, Cmd.none )

        LoginRequested ->
            ( { model | command = "" }, sendLoginRequest model.command )

        CommandSubmitted ->
            ( { model | command = "" }
            , case model.username of
                Just username ->
                    sendCommand username model.command

                Nothing ->
                    Cmd.none
            )

        LoginSucceeded username ->
            ( { model | username = Just username }, Cmd.none )

        MaybeCommandReturn key ->
            case key of
                13 ->
                    ( { model | command = "" }
                    , case model.username of
                        Just username ->
                            sendCommand username model.command

                        Nothing ->
                            Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        MaybeLoginReturn key ->
            case key of
                13 ->
                    ( { model | command = "" }, sendLoginRequest model.command )

                _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html.Html Msg
view model =
    Bootstrap.container
        [ Bootstrap.row
            [ Bootstrap.col 12
                [ h1 [] [ text "Rob's MUD" ]
                ]
            ]
        , Bootstrap.row
            [ Bootstrap.col 12
                <| case model.username of
                    Nothing ->
                        loginView model

                    Just username ->
                        commandView model
            ]
        , Bootstrap.row
            [ Bootstrap.col 12 <| historyView model
            ]
        ]


loginView : Model -> List (Html.Html Msg)
loginView model =
    [ h5 [] [ text "Who are you?" ]
    , input [ type' "text", value model.command, class "form-control", id "login", onInput CommandChanged, onKeyPress MaybeLoginReturn, autofocus True ] []
    , button [ onClick LoginRequested ] [ text "login plzkthx" ]
    ]


commandView : Model -> List (Html.Html Msg)
commandView model =
    [ h5 [] [ text "What now? Brave Adventurer" ]
    , input [ type' "text", value model.command, class "form-control", id "command", onInput CommandChanged, onKeyPress MaybeCommandReturn, autofocus True ] []
    , button [ onClick CommandSubmitted ] [ text "Send" ]
    ]


historyView : Model -> List (Html.Html Msg)
historyView model =
    [ ul [] <| List.map historyItem model.history
    ]


historyItem : String -> Html.Html Msg
historyItem model =
    li [] [ text model ]


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


sendLoginRequest username =
    Task.perform FetchFail LoginSucceeded
        <| Api.login username


sendCommand username command =
    Task.perform FetchFail CommandResult
        <| Api.sendCommand username command
