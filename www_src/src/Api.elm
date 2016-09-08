module Api
    exposing
        ( login
        , sendCommand
        )

import Task
import Date
import Http
import Json.Decode as Json exposing (string, int, list, maybe, bool, tuple2, customDecoder, at)
import Json.Decode.Pipeline exposing (required, decode, optional)


type alias CommandResult =
    {}


login : String -> Platform.Task Http.Error String
login username =
    Http.getString ("login/" ++ username)


sendCommand : String -> String -> Platform.Task Http.Error String
sendCommand username command =
    let
        default =
            Http.defaultSettings
    in
        mapCommandResult
            <| Http.send { default | desiredResponseType = (Just "text/plain") }
                { verb = "POST"
                , headers =
                    [ ( "Content-Type", "application/x-www-form-urlencoded" )
                    ]
                , url = "command/" ++ username
                , body = Http.string <| "command=" ++ command
                }


mapCommandResult : Platform.Task Http.RawError Http.Response -> Platform.Task Http.Error String
mapCommandResult t =
    Task.mapError promoteError (Task.map mapResponse t)


mapResponse : Http.Response -> String
mapResponse response =
    case response.value of
        Http.Blob _ ->
            ""

        Http.Text t ->
            t


promoteError : Http.RawError -> Http.Error
promoteError rawError =
    case rawError of
        Http.RawTimeout ->
            Http.Timeout

        Http.RawNetworkError ->
            Http.NetworkError
