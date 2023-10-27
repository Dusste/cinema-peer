module Profile exposing (..)

import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes as Attr
import Html.Styled.Events exposing (onClick, onInput)
import Tailwind.Breakpoints as Br
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw
import Types exposing (..)


type alias Model =
    ProfileModel


initModel : Model
initModel =
    { nameValue = "" }


init : ( Model, Cmd Types.ProfileMsg )
init =
    ( initModel, Cmd.none )


update : Types.ProfileMsg -> Model -> ( Model, Cmd Types.ProfileMsg )
update msg model =
    case msg of
        StoreName nameValue ->
            ( { model | nameValue = nameValue }, Cmd.none )


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
        , Html.button [] [ text "Submit" ]
        ]
