module Login exposing (..)

import Browser.Navigation as Nav
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes as Attr
import Html.Styled.Events exposing (onClick, onInput)
import Lamdera exposing (sendToBackend)
import Tailwind.Breakpoints as Br
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw
import Types exposing (..)
import Util



-- import Util exposing (parseEmail)


type alias Model =
    LoginModel


initModel : Model
initModel =
    { emailValue = "", error = Nothing, notification = Nothing }


init : ( Model, Cmd FrontendMsg )
init =
    ( initModel, Cmd.none )


update : Types.LoginMsg -> Model -> ( Model, Cmd Types.LoginMsg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        StoreEmail emailValue ->
            ( { model | emailValue = emailValue }, Cmd.none )

        TryRequestLogin emailValue ->
            case Util.parseEmail emailValue of
                Ok validEmail ->
                    ( { model | emailValue = "", error = Nothing, notification = Just "We've send you an email. Use it to login" }, sendToBackend <| RequestLogin validEmail )

                Err err ->
                    ( { model | error = Just err, notification = Nothing }, Cmd.none )


view : Model -> Html Types.LoginMsg
view model =
    Html.div []
        [ Html.div []
            [ Html.div []
                [ Html.p []
                    [ text "Enter your email to login or sing up" ]
                , Html.div []
                    [ Html.input [ Attr.css [ Tw.form_input ], Attr.placeholder "Type email", onInput StoreEmail ] []
                    , Html.button [ onClick <| TryRequestLogin model.emailValue ] [ text "Login / Sign up" ]
                    ]
                , case model.notification of
                    Just msg ->
                        Html.p [] [ text msg ]

                    Nothing ->
                        text ""
                ]
            ]
        ]
