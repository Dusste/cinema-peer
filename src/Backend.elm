module Backend exposing (..)

import Dict exposing (Dict)
import EmailAddress
import Env
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Lamdera exposing (ClientId, SessionId, broadcast, onConnect, onDisconnect, sendToFrontend)
import Set
import String.Nonempty exposing (NonemptyString(..))
import Time
import Types exposing (..)
import Url


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
        , Time.every 1000 GetTime
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


sendToFePage : SessionId -> PageMsgs -> Cmd backendMsg
sendToFePage sessionId toMsg =
    sendToFrontend sessionId <| UpdateToPages toMsg


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        GotMovies (Err err) ->
            -- TODO shouldn't send err to everyone
            ( model, broadcast <| UpdateToPages <| SearchMsg <| ResponseFetchMovies (Err (buildErrorMessage err)) Dict.empty )

        GotMovies (Ok ( sessionId, movies )) ->
            let
                maybeMovieList =
                    model.connections
                        |> Dict.get sessionId
                        |> Maybe.andThen
                            (\usersSessionId ->
                                Dict.get usersSessionId model.users
                                    |> Maybe.map
                                        .movieLists
                            )
            in
            ( model
            , sendToFePage sessionId (SearchMsg (ResponseFetchMovies (Ok movies) (maybeMovieList |> Maybe.withDefault Dict.empty)))
            )

        LoginTokenSend (Ok ()) ->
            ( model, Cmd.none )

        LoginTokenSend (Err err) ->
            ( model, Cmd.none )

        GetTime time ->
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
        GotSession ->
            let
                maybeUser =
                    model.connections
                        |> Dict.get sessionId
                        |> Maybe.andThen
                            (\usersSessionId ->
                                Dict.get usersSessionId model.users
                            )

                cmd =
                    case maybeUser of
                        Just s ->
                            sendToFrontend sessionId <| ResponseAuth (LoggedIn s) Nothing

                        Nothing ->
                            sendToFrontend sessionId <| ResponseAuth Anonymus Nothing
            in
            ( model, cmd )

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

                updatePendingAuth =
                    Dict.remove tokenString model.pendingAuth

                ( updatedModel, cmdMsg ) =
                    case model.pendingAuth |> Dict.get tokenString of
                        Just { createTime, email } ->
                            if Time.posixToMillis model.currentTime - Time.posixToMillis createTime < 3600000 then
                                -- User have 1 hour window to use login token
                                let
                                    updateConnections =
                                        Dict.insert sessionId sessionId model.connections
                                in
                                case model.users |> Dict.get sessionId of
                                    Just alreadyUser ->
                                        ( { model | pendingAuth = updatePendingAuth, connections = updateConnections }
                                        , sendToFrontend sessionId <| ResponseAuth (LoggedIn alreadyUser) (Just "Welcome back !")
                                        )

                                    Nothing ->
                                        let
                                            generateId =
                                                getId model.currentTime

                                            newUser =
                                                { id = generateId, email = email, name = "", movieLists = Dict.empty }

                                            updateWithNewUser =
                                                Dict.insert sessionId newUser model.users
                                        in
                                        ( { model | users = updateWithNewUser, connections = updateConnections }
                                        , sendToFrontend sessionId <| ResponseAuth (LoggedIn newUser) (Just "Welcome newly registered user !")
                                        )

                            else
                                ( { model | pendingAuth = updatePendingAuth }, sendToFrontend clientId <| ResponseAuth Anonymus (Just "This token has expired") )

                        Nothing ->
                            let
                                updateModel =
                                    { model | pendingAuth = updatePendingAuth }

                                maybeUser =
                                    model.connections
                                        |> Dict.get sessionId
                                        |> Maybe.andThen
                                            (\usersSessionId ->
                                                Dict.get usersSessionId model.users
                                            )
                            in
                            case maybeUser of
                                Just userInfo ->
                                    ( updateModel
                                    , sendToFrontend sessionId <| ResponseAuth (LoggedIn userInfo) (Just "[We are ignoring the fact the you are trying to connect with same token] User already logged in")
                                    )

                                Nothing ->
                                    ( updateModel
                                    , sendToFrontend sessionId <| ResponseAuth Anonymus (Just "Token already used")
                                    )
            in
            ( updatedModel, Cmd.batch [ cmdMsg, sendToFrontend sessionId GoHome ] )

        RequestLogout ->
            ( { model | connections = Dict.remove sessionId model.connections }
            , sendToFrontend sessionId <| ResponseAuth Anonymus (Just "Session has been terminated")
            )

        RequestUpdateName name ->
            let
                maybeUpdatedUsers =
                    model.connections
                        |> Dict.get sessionId
                        |> Maybe.map
                            (\usersSessionId ->
                                let
                                    updateUser =
                                        Maybe.map (\u -> { u | name = name })
                                in
                                Dict.update usersSessionId updateUser model.users
                            )
            in
            case maybeUpdatedUsers of
                Just updatedUsers ->
                    ( { model | users = updatedUsers }
                    , case Dict.get sessionId updatedUsers of
                        Just user ->
                            sendToFrontend sessionId <| ResponseAuth (LoggedIn user) (Just "Name successfully saved !")

                        Nothing ->
                            Cmd.none
                    )

                Nothing ->
                    ( model, sendToFrontend sessionId <| ResponseAuth Anonymus (Just "Your session is not valid") )

        RequestNewList newListName ->
            addNewListAndMovie
                { sessionId = sessionId
                , model = model
                , newListName = newListName
                , notification = "New list '" ++ newListName ++ "' created !"
                , lstOfMovies = []
                }

        -- FetchMovieLists ->
        --     let
        --         maybeMovieList =
        --             model.connections
        --                 |> Dict.get sessionId
        --                 |> Maybe.andThen
        --                     (\usersSessionId ->
        --                         Dict.get usersSessionId model.users
        --                             |> Maybe.map
        --                                 .movieLists
        --                     )
        --     in
        --     case maybeMovieList of
        --         Just foundMovieLists ->
        --             ( model, sendToFePage sessionId <| SearchMsg <| ResponseUsersMovieLists foundMovieLists )
        --         Nothing ->
        --             ( model, Cmd.none )
        RequestWriteMovieInNewLists ( newListName, selectedMovie ) ->
            addNewListAndMovie
                { sessionId = sessionId
                , model = model
                , newListName = newListName
                , notification = "Movie '" ++ selectedMovie.title ++ "' added to NEW list '" ++ newListName ++ "'!"
                , lstOfMovies = [ selectedMovie ]
                }

        RequestWriteLists ( selectedMovie, targetListIds ) ->
            let
                toGetUser : User -> ( BackendModel, Cmd BackendMsg )
                toGetUser currentUser =
                    let
                        updateUser =
                            Maybe.map (\u -> { u | movieLists = updatedMovieLists currentUser.movieLists selectedMovie targetListIds })

                        updatedUsers =
                            Dict.update sessionId updateUser model.users
                    in
                    ( { model | users = updatedUsers }
                    , case Dict.get sessionId updatedUsers of
                        Just user ->
                            sendToFrontend sessionId <| ResponseAuth (LoggedIn user) (Just "List of movies successfully updated !")

                        Nothing ->
                            Cmd.none
                    )

                toGetConnectedUser : SessionId -> Maybe ( BackendModel, Cmd BackendMsg )
                toGetConnectedUser usersSessionId =
                    Dict.get usersSessionId model.users
                        |> Maybe.map
                            toGetUser

                maybeUpdatedModelCmdMsg : Maybe ( BackendModel, Cmd BackendMsg )
                maybeUpdatedModelCmdMsg =
                    model.connections
                        |> Dict.get sessionId
                        |> Maybe.andThen
                            toGetConnectedUser
            in
            case maybeUpdatedModelCmdMsg of
                Just ( updatedModel, cmdMsg ) ->
                    ( updatedModel, cmdMsg )

                Nothing ->
                    ( model, sendToFrontend sessionId <| ResponseAuth Anonymus (Just "Your session is not valid") )


addNewListAndMovie :
    { sessionId : SessionId, model : Model, newListName : MovieListName, notification : String, lstOfMovies : List Movie }
    -> ( Model, Cmd BackendMsg )
addNewListAndMovie { sessionId, model, newListName, notification, lstOfMovies } =
    case Dict.get sessionId model.connections of
        Just usersSessionId ->
            let
                insertNewList u =
                    Dict.insert newListName { listId = generatedListId, sharedWith = Set.empty, listOfMovies = lstOfMovies } u.movieLists

                updateUser =
                    Maybe.map (\u -> { u | movieLists = insertNewList u })

                generatedListId =
                    generateNewListId model.currentTime

                updatedUsers =
                    Dict.update usersSessionId updateUser model.users
            in
            ( { model | users = updatedUsers }
            , case Dict.get sessionId updatedUsers of
                Just user ->
                    sendToFrontend sessionId <| ResponseAuth (LoggedIn user) (Just notification)

                Nothing ->
                    Cmd.none
            )

        Nothing ->
            ( model, sendToFrontend sessionId <| ResponseAuth Anonymus (Just "Your session is not valid") )
