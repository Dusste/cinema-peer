module MovieList exposing (..)

import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes as Attr
import Html.Styled.Events exposing (onCheck, onClick, onInput)
import Lamdera exposing (sendToBackend)
import Tailwind.Breakpoints as Br
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw
import Types exposing (..)


type alias Model =
    MovieListModel


initModel : Model
initModel =
    {}


init : ( Model, Cmd MovieListMsg )
init =
    ( initModel, Cmd.none )


update : MovieListMsg -> Model -> ( Model, Cmd MovieListMsg )
update msg model =
    case msg of
        NoOpM ->
            ( model, Cmd.none )


view : Model -> Html MovieListMsg
view model =
    Html.div [] [ text "this is movie list page !" ]
