module Search exposing (..)

import Dict exposing (Dict)
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes as Attr
import Html.Styled.Events exposing (onCheck, onClick, onInput)
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
    , movieModalState = Closed
    , movieLists = Dict.empty
    , newListName = ""
    , selectedMovies = []
    }


fromWordToInt : String -> Maybe Int
fromWordToInt wrd =
    case wrd of
        "one" ->
            Just 1

        "two" ->
            Just 2

        "three" ->
            Just 3

        "four" ->
            Just 4

        "five" ->
            Just 5

        "six" ->
            Just 6

        "seven" ->
            Just 7

        "eight" ->
            Just 8

        "nine" ->
            Just 9

        _ ->
            Nothing


init : ( Model, Cmd SearchMsg )
init =
    ( initModel, Cmd.none )


update : SearchMsg -> Model -> ( Model, Cmd SearchMsg )
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

        StoreNewListFromSearch str ->
            ( { model | newListName = str }, Cmd.none )

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

        ResponseFetchMovies (Ok movies) movieLists ->
            ( { model | resultsState = Success movies, timer = TimerOff, movieLists = movieLists }, Cmd.none )

        ResponseFetchMovies (Err err) _ ->
            ( { model | timer = TimerOff }, Cmd.none )

        OpenModal movieModalState ->
            ( { model | movieModalState = movieModalState, selectedMovies = [] }, Cmd.none )

        -- ( { model | movieModalState = movieModalState, selectedMovies = [] }, sendToBackend FetchMovieLists )
        -- ResponseUsersMovieLists movieLists ->
        --     ( { model | movieLists = movieLists }, Cmd.none )
        CreateNewListFromSearch ->
            let
                trimmedListName =
                    String.trim model.newListName
            in
            if String.isEmpty trimmedListName then
                ( model, Cmd.none )

            else
                ( { model | movieModalState = Closed }
                , case model.movieModalState of
                    Opened movie ->
                        sendToBackend <| RequestWriteMovieInNewLists ( trimmedListName, movie )

                    Closed ->
                        Cmd.none
                )

        -- ShouldKeepMovie shouldKeep ->
        CheckList listId shouldKeep ->
            let
                computedListOfLists lst =
                    if shouldKeep then
                        listId :: lst

                    else
                        List.filter (\listId_ -> listId_ /= listId) lst
            in
            ( { model | selectedMovies = computedListOfLists model.selectedMovies }, Cmd.none )

        InitWriteLists ->
            ( { model | selectedMovies = [] }
            , case model.movieModalState of
                Opened selectedMovie ->
                    sendToBackend <| RequestWriteLists ( selectedMovie, model.selectedMovies )

                Closed ->
                    Cmd.none
            )



-- GotBeSearchMsg resultsState ->
--     ( { model | resultsState = resultsState, timer = TimerOff }, Cmd.none )
-- ResponseFetchMovies (Err _) ->
--     ( { model | timer = TimerOff }, Cmd.none )
-- ResponseFetchMovies (Ok movies) ->
--     ( { model | resultsState = Success movies, timer = TimerOff }, Cmd.none )


view : Model -> Html SearchMsg
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
                        movies |> List.map (viewEachMovie model.movieModalState model.movieLists model.selectedMovies)
                    )
        ]


viewEachMovie : MovieModalState -> Dict MovieListName MovieListData -> List ListId -> Movie -> Html SearchMsg
viewEachMovie movieModalState movieLists selectedMovies movie =
    Html.li []
        [ viewEachMovieContent movie
        , case movieModalState of
            Opened activeMovie ->
                if activeMovie.id == movie.id then
                    Html.div []
                        [ Html.p [] [ text <| "Where do you want to add " ++ activeMovie.title ++ (activeMovie.id |> Debug.toString) ++ "?" ]
                        , Html.div []
                            [ let
                                movieListToList =
                                    movieLists |> Dict.toList
                              in
                              if List.isEmpty movieListToList then
                                viewModalEmptyList

                              else
                                viewModalMovieList movieListToList selectedMovies
                            ]
                        ]

                else
                    text ""

            Closed ->
                text ""
        ]


viewEachMovieContent : Movie -> Html SearchMsg
viewEachMovieContent movie =
    Html.div []
        [ Html.img [ Attr.src <| "https://image.tmdb.org/t/p/original" ++ movie.posterPath, Attr.css [ Tw.w_24 ] ] []
        , text movie.title
        , Html.button [ onClick <| OpenModal (Opened movie) ] [ text "add to the list" ]
        ]


viewModalMovieList : List ( MovieListName, MovieListData ) -> List ListId -> Html SearchMsg
viewModalMovieList movieListToList selectedMovies =
    Html.div []
        [ Html.ul []
            (movieListToList
                |> List.map
                    (\( listName, { listId } ) ->
                        Html.li []
                            [ Html.div []
                                [ Html.p [] [ text listName ]
                                , Html.input
                                    [ Attr.type_ "checkbox"
                                    , onCheck (CheckList listId)
                                    ]
                                    []
                                ]
                            ]
                    )
            )
        , if List.isEmpty selectedMovies then
            text ""

          else
            Html.button [ onClick InitWriteLists ] [ text "Add" ]
        ]


viewModalEmptyList : Html SearchMsg
viewModalEmptyList =
    Html.div [ Attr.css [ Tw.absolute ] ]
        [ Html.p [] [ text "Add movie to your new list ?" ]
        , Html.div []
            [ Html.input [ Attr.type_ "text", onInput StoreNewListFromSearch ] []
            , Html.button [ onClick CreateNewListFromSearch ] [ text "Submit" ]
            ]
        ]
