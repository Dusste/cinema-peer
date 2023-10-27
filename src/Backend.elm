module Backend exposing (..)

import Dict
import EmailAddress exposing (EmailAddress)
import Env
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Lamdera exposing (ClientId, SessionId, broadcast, onConnect, onDisconnect, sendToFrontend)
import Lamdera.Wire exposing (encodeTriple)
import String.Nonempty exposing (NonemptyString(..))
import Task
import Time
import Types exposing (..)
import Url
import Util


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


subscriptions : BackendModel -> Sub BackendMsg
subscriptions _ =
    Sub.batch
        [ onConnect UserConnected
        , onDisconnect UserDisconnected
        , Time.every 10000 GetTime
        ]


init : ( Model, Cmd BackendMsg )
init =
    ( { users = Dict.empty
      , connections = Dict.empty
      , currentTime = Time.millisToPosix 0
      , pendingAuth = Dict.empty
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        GotMovies (Err err) ->
            -- TODO shouldn't send err to everyone
            ( model, broadcast <| ResponseFetchMovies (Err (buildErrorMessage err)) )

        GotMovies (Ok ( sessionId, movies )) ->
            ( model, sendToFrontend sessionId <| ResponseFetchMovies (Ok movies) )

        LoginTokenSend (Ok ()) ->
            ( model, Cmd.none )

        LoginTokenSend (Err err) ->
            let
                _ =
                    Debug.log "Dusan" err
            in
            ( model, Cmd.none )

        GetTime time ->
            let
                _ =
                    Debug.log "GET TIME" Time.posixToMillis
            in
            ( { model | currentTime = time }, Cmd.none )

        UserConnected sessionId clientId ->
            ( model, Cmd.none )

        -- ( { model | connections = Dict.insert sessionId Anonymus model.connections }, Cmd.none )
        UserDisconnected sessionId clientId ->
            ( model, Cmd.none )



-- ( { model | connections = Dict.remove sessionId model.connections }, Cmd.none )


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message


fetchMovie : String -> SessionId -> Cmd BackendMsg
fetchMovie movie sessionId =
    Http.request
        { url = "https://api.themoviedb.org/3/search/movie?query=" ++ Url.percentEncode movie ++ "&include_adult=false&language=en-US&page=1"
        , method = "GET"
        , headers = [ Http.header "authorization" <| "Bearer " ++ Env.apiToken ]
        , body = Http.emptyBody
        , expect = Http.expectJson GotMovies (moviesDecoder sessionId)
        , tracker = Nothing
        , timeout = Nothing
        }


movieDecoder : Decoder Movie
movieDecoder =
    Decode.succeed Movie
        |> required "id" Decode.int
        |> required "overview" Decode.string
        |> optional "poster_path" Decode.string "Poster not available"
        |> required "release_date" Decode.string
        |> required "title" Decode.string


moviesDecoder : SessionId -> Decoder ( SessionId, List Movie )
moviesDecoder sessionId =
    Decode.succeed (\movieList -> ( sessionId, movieList ))
        |> required "results" (Decode.list movieDecoder)


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        GotSession ->
            let
                session =
                    case model.connections |> Dict.get sessionId of
                        Just usersSessionId ->
                            case Dict.get usersSessionId model.users of
                                Just userInfo ->
                                    LoggedIn userInfo

                                Nothing ->
                                    Anonymus

                        Nothing ->
                            Anonymus
            in
            ( model, sendToFrontend sessionId <| ResponseAuth session "" )

        -- TODO make it Maybe
        RequestFetchMovies movie ->
            ( model, fetchMovie movie sessionId )

        RequestLogin (Email email) ->
            case ( EmailAddress.fromString email, EmailAddress.fromString Env.senderEmail ) of
                ( Just toEmail, Just fromEmail ) ->
                    let
                        token =
                            Types.getLoginToken model.currentTime

                        config =
                            Types.getConfig toEmail fromEmail token
                    in
                    ( { model | pendingAuth = Types.insertPendingAuth token model (Email email) }, Types.sendEmailToUser config )

                _ ->
                    ( model, Cmd.none )

        RequestAuth token ->
            let
                tokenString =
                    tokenToString token

                _ =
                    Debug.log "PENDING AUTH: " model.pendingAuth

                _ =
                    Debug.log "incoming token:" tokenString

                updatePendingAuth =
                    Dict.remove tokenString model.pendingAuth
            in
            -- TODO its a mess - clean it up !
            case model.pendingAuth |> Dict.get tokenString of
                Just { createTime, email } ->
                    let
                        _ =
                            Debug.log "create email" { createTime = createTime, currentTime = model.currentTime }
                    in
                    if Time.posixToMillis model.currentTime - Time.posixToMillis createTime < 3600000 then
                        let
                            updateConnections =
                                Dict.insert sessionId sessionId model.connections
                        in
                        case model.users |> Dict.get sessionId of
                            Just alreadyUser ->
                                let
                                    _ =
                                        Debug.log "alreadyUser" alreadyUser

                                    _ =
                                        Debug.log "Newly updated connection: " updateConnections
                                in
                                ( { model | pendingAuth = updatePendingAuth, connections = updateConnections }
                                , sendToFrontend sessionId <| ResponseAuth (LoggedIn alreadyUser) "Welcome back !"
                                )

                            Nothing ->
                                let
                                    _ =
                                        Debug.log "its a newly registered user" ""

                                    id =
                                        getId model.currentTime

                                    newUser =
                                        { id = id, email = email, name = "", movieLists = [] }

                                    updateWithNewUser =
                                        -- TODO do we need ID ?
                                        Dict.insert sessionId newUser model.users
                                in
                                -- Todo - what should be send to new user ?
                                ( { model | users = updateWithNewUser, connections = updateConnections }
                                , sendToFrontend sessionId <| ResponseAuth (LoggedIn newUser) "Welcome newly registered user !"
                                )

                    else
                        ( { model | pendingAuth = updatePendingAuth }, sendToFrontend clientId <| ResponseAuth Anonymus "This token has expired" )

                Nothing ->
                    let
                        updateModel =
                            { model | pendingAuth = updatePendingAuth }
                    in
                    case model.connections |> Dict.get sessionId of
                        Just usersSessionId ->
                            case Dict.get usersSessionId model.users of
                                Just userInfo ->
                                    ( updateModel
                                    , sendToFrontend sessionId <| ResponseAuth (LoggedIn userInfo) "[We are ignoring the fact the you are trying to connect with same token] User already logged in"
                                    )

                                Nothing ->
                                    ( updateModel
                                    , sendToFrontend sessionId <| ResponseAuth Anonymus "Token already used"
                                    )

                        Nothing ->
                            ( updateModel
                            , sendToFrontend sessionId <| ResponseAuth Anonymus "Token already used"
                            )

        -- Nothing ->
        --     ( model, sendToFrontend clientId InvalidToken )
        RequestLogout ->
            ( { model | connections = Dict.remove sessionId model.connections }
            , sendToFrontend sessionId <| ResponseAuth Anonymus "Session has been terminated"
            )

        RequestUpdateName name ->
            let
                updateUser =
                    Maybe.map (\u -> { u | name = name })

                updatedUsers =
                    Dict.update sessionId updateUser model.users
            in
            ( { model | users = updatedUsers }
            , case Dict.get sessionId updatedUsers of
                Just user ->
                    sendToFrontend sessionId <| ResponseUserUpdate user

                Nothing ->
                    Cmd.none
            )
