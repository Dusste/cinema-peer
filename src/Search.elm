module Search exposing (..)

import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes as Attr
import Html.Styled.Events exposing (onClick, onInput)
import Lamdera exposing (sendToBackend)
import Tailwind.Breakpoints as Br
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw
import Types exposing (..)


type alias Model =
    SearchModel


initModel : Model
initModel =
    { movieValue = ""
    , resultsState = Initial
    , timer = TimerOff
    }


init : ( Model, Cmd Types.SearchMsg )
init =
    ( initModel, Cmd.none )


update : Types.SearchMsg -> Model -> ( Model, Cmd Types.SearchMsg )
update msg model =
    case msg of
        StoreMovie movieValue ->
            ( { model
                | movieValue = movieValue
                , timer =
                    if String.isEmpty movieValue then
                        TimerOff

                    else
                        TimerOn 0
                , resultsState =
                    if String.isEmpty movieValue then
                        Initial

                    else
                        Loading
              }
            , Cmd.none
            )

        Tick _ ->
            case model.timer of
                TimerOff ->
                    ( model, Cmd.none )

                TimerOn timerInSeconds ->
                    ( { model | timer = TimerOn <| timerInSeconds + 1, resultsState = Loading }
                    , if model.timer == TimerOn 2 && String.length model.movieValue >= 3 then
                        sendToBackend <| RequestFetchMovies model.movieValue

                      else
                        Cmd.none
                    )

        ResponseFetchMovies (Ok movies) ->
            ( { model | resultsState = Success movies, timer = TimerOff }, Cmd.none )

        ResponseFetchMovies (Err err) ->
            ( { model | timer = TimerOff }, Cmd.none )



-- GotBeSearchMsg resultsState ->
--     ( { model | resultsState = resultsState, timer = TimerOff }, Cmd.none )
-- ResponseFetchMovies (Err _) ->
--     ( { model | timer = TimerOff }, Cmd.none )
-- ResponseFetchMovies (Ok movies) ->
--     ( { model | resultsState = Success movies, timer = TimerOff }, Cmd.none )


view : Model -> Html Types.SearchMsg
view model =
    Html.div []
        [ Html.h2 [] [ text "Whanna search a movie ?" ]
        , Html.form []
            [ Html.input
                [ Attr.css [ Tw.form_input ]
                , Attr.placeholder "Type name of the movie"
                , onInput StoreMovie
                ]
                []
            ]
        , case model.resultsState of
            Initial ->
                text ""

            Loading ->
                Html.div [] [ text "Loading ..." ]

            Success movies ->
                Html.ul []
                    (if List.isEmpty movies then
                        [ Html.div [] [ text "Hmm couldn't find any movie" ] ]

                     else
                        movies |> List.map (\movie -> Html.li [] [ Html.img [ Attr.src <| "https://image.tmdb.org/t/p/original" ++ movie.posterPath, Attr.css [ Tw.w_24 ] ] [], text movie.title ])
                    )
        ]
