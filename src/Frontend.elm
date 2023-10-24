module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav exposing (Key)
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes as Attr
import Html.Styled.Events exposing (onClick, onInput)
import Lamdera exposing (sendToBackend)
import Login
import Platform.Cmd as Cmd
import Profile
import Search
import Tailwind.Breakpoints as Br
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw
import Time
import Types exposing (..)
import Url exposing (Url)
import Url.Parser as UrlP exposing ((</>), (<?>), custom, s)
import Url.Parser.Query as UrlPQ


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }


init : Url -> Key -> ( Model, Cmd FrontendMsg )
init url key =
    let
        requestToBackend =
            let
                _ =
                    Debug.log "requestToBackend" (UrlP.parse matchRoute url)
            in
            case UrlP.parse matchRoute url of
                Just (LoginToken maybeToken) ->
                    let
                        _ =
                            Debug.log "usao u login" maybeToken
                    in
                    case maybeToken of
                        Just token ->
                            Cmd.batch [ sendToBackend <| RequestAuth token, Nav.replaceUrl key "/" ]

                        Nothing ->
                            Cmd.none

                _ ->
                    sendToBackend GotSession
    in
    ( { key = key
      , sessionStatus = IsLoading
      , error = Nothing
      , page = urlToPage url
      }
    , requestToBackend
    )


matchRoute : UrlP.Parser (Route -> a) a
matchRoute =
    UrlP.oneOf
        [ UrlP.map Home UrlP.top
        , UrlP.map LoginToken (s "loginToken" <?> parserToken)
        , UrlP.map Login (s "login")
        , UrlP.map Search (s "search")
        , UrlP.map Profile (s "profile" </> userIdParser)
        ]


urlToPage : Url -> Page
urlToPage url =
    case UrlP.parse matchRoute url of
        Just Home ->
            HomePage

        Just (LoginToken _) ->
            HomePage

        Just Login ->
            LoginPage (Login.init |> Tuple.first)

        Just (Profile _) ->
            ProfilePage (Profile.init |> Tuple.first)

        Just Search ->
            SearchPage (Search.init |> Tuple.first)

        _ ->
            NotFoundPage


userIdParser : UrlP.Parser (UserId -> a) a
userIdParser =
    custom "USERID"
        (\userId ->
            Maybe.map UserId (Just userId)
        )


parserToken : UrlPQ.Parser (Maybe Token)
parserToken =
    UrlPQ.string "token"
        |> UrlPQ.map (Maybe.map Token)


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( { model | page = urlToPage url }
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( { model | page = urlToPage url }, Cmd.none )

        TriggerLogout ->
            ( { model | sessionStatus = Anonymus }, sendToBackend RequestLogout )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        GotLoginMsg loginMsg ->
            case model.page of
                LoginPage loginModel ->
                    let
                        ( modelFromLogin, cmdMsgFromLogin ) =
                            Login.update loginMsg loginModel
                    in
                    ( { model | page = LoginPage modelFromLogin }, Cmd.map GotLoginMsg cmdMsgFromLogin )

                _ ->
                    ( model, Cmd.none )

        GotSearchMsg searchMsg ->
            case model.page of
                SearchPage searchModel ->
                    let
                        ( modelFromSearch, cmdMsgFromSearch ) =
                            Search.update searchMsg searchModel
                    in
                    ( { model | page = SearchPage modelFromSearch }, Cmd.map GotSearchMsg cmdMsgFromSearch )

                _ ->
                    ( model, Cmd.none )

        GotProfileMsg profileMsg ->
            case model.page of
                ProfilePage profileModel ->
                    let
                        ( modelFromProfile, cmdMsgFromProfile ) =
                            Profile.update profileMsg profileModel
                    in
                    ( { model | page = ProfilePage modelFromProfile }, Cmd.map GotProfileMsg cmdMsgFromProfile )

                _ ->
                    ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        ResponseAuth sessionStatus notification ->
            let
                _ =
                    Debug.log "NOTIFICATION FROM BE: " notification
            in
            ( { model | sessionStatus = sessionStatus }, Nav.replaceUrl model.key "/" )

        ResponseFetchMovies (Ok movies) ->
            case model.page of
                SearchPage searchModel ->
                    let
                        ( modelFromSearch, cmdMsgFromSearch ) =
                            Search.update (GotBeSearchMsg (Success movies)) searchModel
                    in
                    ( { model | page = SearchPage modelFromSearch }, Cmd.map GotSearchMsg cmdMsgFromSearch )

                _ ->
                    ( model, Cmd.none )

        -- ( model, Cmd.map GotSearchResults (Success movies) )
        ResponseFetchMovies (Err err) ->
            ( model, Cmd.none )



-- ( { model | resultsState = Success movies, timer = TimerOff }, Cmd.none )
-- GotBeSearchMsg searchMsg ->
--     case model.page of
--         SearchPage searchModel ->
--             let
--                 ( modelFromSearch, cmdMsgFromSearch ) =
--                     Search.update searchMsg searchModel
--             in
--             ( { model | page = SearchPage modelFromSearch }, Cmd.map GotSearchMsg cmdMsgFromSearch )
--         _ ->
--             ( model, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "CinemaPeer"
    , body =
        [ Html.toUnstyled <|
            content model
        ]
    }


content : Model -> Html FrontendMsg
content model =
    case model.sessionStatus of
        IsLoading ->
            text "LOADING...."

        _ ->
            Html.div [ Attr.css [ Tw.flex, Tw.flex_col ] ]
                [ header model
                , errorMsg model.error
                , Html.div []
                    [ case model.page of
                        HomePage ->
                            Html.div []
                                [ Html.div []
                                    [ Html.h1 [] [ text "Cinema peer" ]
                                    , Html.p [] [ text "Share movies with your friends" ]
                                    ]
                                , Html.div []
                                    [ Html.h4 [] [ text "How does it works ?" ]
                                    , Html.ul [ Attr.css [ Tw.flex, Tw.list_none, Tw.flex_col, Tw.p_0, Tw.m_0 ] ]
                                        [ Html.li [] [ text "- Create new movie list that you would like to share with a friend" ]
                                        , Html.li [] [ text "- Find movie that you want to recommend to that friend" ]
                                        , Html.li [] [ text "- Add movie to that list" ]
                                        , Html.li [] [ text "- Copy list URL" ]
                                        , Html.li [] [ text "- Share list with your friend" ]
                                        ]
                                    ]
                                ]

                        SearchPage searchModel ->
                            Search.view searchModel |> Html.map GotSearchMsg

                        ProfilePage profileModel ->
                            Profile.view profileModel |> Html.map GotProfileMsg

                        LoginPage loginModel ->
                            Login.view loginModel |> Html.map GotLoginMsg

                        NotFoundPage ->
                            Html.div [] [ text "404 page" ]
                    ]
                ]


errorMsg error =
    case error of
        Just err ->
            Html.div [] [ text err ]

        Nothing ->
            text ""


header model =
    Html.nav []
        [ Html.a [ Attr.href "/" ] [ text "home" ]
        , Html.a [ Attr.href "/search" ] [ text "search a movie" ]
        , case model.sessionStatus of
            LoggedIn _ ->
                Html.div []
                    [ Html.a [ Attr.href "", onClick TriggerLogout ] [ text "logout" ]
                    , Html.a [ Attr.href "/profile/123" ] [ text "profile" ] -- TODO profile id hardcoded
                    ]

            Anonymus ->
                Html.a [ Attr.href "/login" ] [ text "login / signup" ]

            IsLoading ->
                text ""
        ]


subscriptions : Model -> Sub FrontendMsg
subscriptions m =
    case m.page of
        SearchPage searchModel ->
            case searchModel.timer of
                TimerOff ->
                    Sub.none

                TimerOn _ ->
                    Time.every 1000 Tick |> Sub.map GotSearchMsg

        _ ->
            Sub.none
