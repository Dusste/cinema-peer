module Profile exposing (..)

import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes as Attr
import Html.Styled.Events exposing (onClick, onInput)
import Lamdera exposing (sendToBackend)
import Tailwind.Breakpoints as Br
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw
import Types exposing (..)


type alias Model =
    ProfileModel


initModel : Model
initModel =
    { nameValue = "" }


init : Maybe String -> ( Model, Cmd Types.ProfileMsg )
init maybeName =
    let
        _ =
            Debug.log "Profile.init " maybeName
    in
    ( { nameValue = maybeName |> Maybe.withDefault "" }, Cmd.none )


update : Types.ProfileMsg -> Model -> ( Model, Cmd Types.ProfileMsg )
update msg model =
    case msg of
        StoreName nameValue ->
            ( { model | nameValue = nameValue }, Cmd.none )

        -- ResponseUserUpdate { name } ->
        --     let
        --         _ =
        --             Debug.log "usao u ResponseUserUpdate in PRfile " name
        --     in
        -- ( { model | nameValue = name }, Cmd.none )
        SubmitName ->
            ( model, sendToBackend <| RequestUpdateName model.nameValue )



-- transformProfileMsgToFeMsg : (a -> ProfileMsg) -> b -> Cmd FrontendMsg
-- transformProfileMsgToFeMsg toMsg payload =
--     sendToBackend <| payload toMsg
-- _ ->
--     ( model, Cmd.none )
-- updateBeFromProfile : ProfileMsg -> Model -> Cmd FrontendMsg
-- updateBeFromProfile profileMsg profileModel =
--     case profileMsg of
--         SubmitName ->
--             sendToBackend <| RequestUpdateName profileModel.nameValue
--         _ ->
--             Cmd.none
-- updateProfileFromBe : ProfileMsg -> ProfileModel -> Cmd FrontendMsg
-- updateProfileFromBe profileMsg profileModel =
--     case profileMsg of
--         GotBeProfileMsg ->
--             sendToBackend <| RequestUpdateName profileModel.nameValue
--         _ ->
--             Cmd.none


view : Model -> Html Types.ProfileMsg
view model =
    Html.div []
        [ Html.h2 [] [ text "Some stuff about you" ]
        , Html.form []
            [ Html.fieldset []
                [ Html.label [ Attr.for "email" ]
                    [ text "Name"
                    , Html.input
                        [ Attr.id "email"
                        , onInput StoreName
                        , Attr.value model.nameValue
                        ]
                        []
                    ]
                ]
            ]
        , Html.button [ onClick SubmitName ] [ text "Submit" ]
        ]
