module MovieList exposing (..)

import Dict exposing (Dict)
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes as Attr
import Html.Styled.Events exposing (onCheck, onClick, onInput)
import Lamdera exposing (sendToBackend)
import Set exposing (Set)
import Tailwind.Breakpoints as Br
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw
import Types exposing (..)


type alias Model =
    MovieListModel


initModel : Model
initModel =
    { movieList = Dict.empty }


init : Dict MovieListName MovieListData -> ( Model, Cmd MovieListMsg )
init maybeMovieList =
    ( { initModel | movieList = maybeMovieList }, Cmd.none )


update : MovieListMsg -> Model -> ( Model, Cmd MovieListMsg )
update msg model =
    case msg of
        NoOpMovieList ->
            ( model, Cmd.none )


view : Model -> Html MovieListMsg
view model =
    Html.div []
        (model.movieList
            |> Dict.toList
            |> List.map
                (\( listName, listData ) ->
                    Html.div []
                        [ Html.h3 [] [ text listName ]
                        , Html.div []
                            [ Html.ul []
                                (listData.listOfMovies
                                    |> List.map
                                        (\m ->
                                            Html.li []
                                                [ Html.div []
                                                    [ Html.div []
                                                        [ Html.img [ Attr.src <| "https://image.tmdb.org/t/p/original" ++ m.posterPath, Attr.css [ Tw.w_24 ] ] [] ]
                                                    , Html.div []
                                                        [ Html.p [] [ text "Movie Name:" ]
                                                        , Html.p [] [ text m.title ]
                                                        ]
                                                    , Html.div []
                                                        [ Html.p [] [ text "Movie Overview:" ]
                                                        , Html.p [] [ text m.overview ]
                                                        ]
                                                    , Html.div []
                                                        [ Html.p [] [ text "Movie Release Date:" ]
                                                        , Html.p [] [ text m.releaseDate ]
                                                        ]
                                                    ]
                                                ]
                                        )
                                )
                            , Html.div []
                                [ Html.p [] [ text "Who can access this list ?" ]
                                , Html.ul [] (listData.sharedWith |> Set.toList |> List.map (\email -> Html.li [] [ text email ]))
                                ]
                            ]
                        ]
                )
        )
