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
    { emailValue = "" }


init : ( Model, Cmd Types.ProfileMsg )
init =
    ( initModel, Cmd.none )


update : Types.ProfileMsg -> Model -> ( Model, Cmd Types.ProfileMsg )
update msg model =
    case msg of
        Nja ->
            ( model, Cmd.none )


view : Model -> Html Types.ProfileMsg
view model =
    Html.div [] [ text "Some stuff about me" ]
