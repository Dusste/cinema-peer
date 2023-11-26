module Profile exposing (..)

import Dict exposing (Dict)
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes as Attr
import Html.Styled.Events exposing (onClick, onInput)
import Lamdera exposing (sendToBackend)
import Set
import Tailwind.Breakpoints as Br
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw
import Types exposing (..)
import Url exposing (Url)


type alias Model =
    ProfileModel


initModel : Model
initModel =
    { name = "", movieLists = Dict.empty, newListName = "", id = Nothing }


init : User -> ( Model, Cmd Types.ProfileMsg )
init { name, movieLists, id } =
    ( { name = name, movieLists = movieLists, newListName = "", id = Just id }, Cmd.none )


update : Types.ProfileMsg -> Model -> ( Model, Cmd Types.ProfileMsg )
update msg model =
    case msg of
        StoreName nameValue ->
            ( { model | name = nameValue }, Cmd.none )

        SubmitName ->
            ( model, sendToBackend <| RequestUpdateName model.name )

        StoreNewListFromProfile str ->
            ( { model | newListName = str }, Cmd.none )

        CreateNewListFromProfile ->
            let
                trimmedListName =
                    String.trim model.newListName
            in
            if String.isEmpty trimmedListName then
                ( model, Cmd.none )

            else
                ( { model | newListName = "" }, sendToBackend <| RequestNewList trimmedListName )


view : Model -> Html Types.ProfileMsg
view model =
    Html.div []
        [ Html.h2 [] [ text "Some stuff about you" ]
        , Html.div []
            [ Html.fieldset []
                [ Html.label [ Attr.for "email" ]
                    [ text "Name"
                    , Html.input
                        [ Attr.id "email"
                        , onInput StoreName
                        , Attr.value model.name
                        ]
                        []
                    ]
                ]
            , Html.button [ onClick SubmitName ] [ text "Submit" ]
            ]
        , Html.div []
            [ Html.h3 [] [ text "Your Movie Lists " ]
            , let
                noLists =
                    model.movieLists |> Dict.isEmpty
              in
              if noLists then
                Html.div []
                    [ Html.p [] [ text "You don't have any movie lists yet. Create one ?" ]
                    , Html.div [ Attr.css [ Tw.absolute ] ]
                        [ Html.p [] [ text "Create new movie list" ]
                        , Html.div []
                            [ Html.input [ Attr.type_ "text", onInput StoreNewListFromProfile ] []
                            , Html.button [ onClick CreateNewListFromProfile ] [ text "Submit" ]
                            ]
                        ]
                    ]

              else
                Html.ul []
                    (model.movieLists
                        |> Dict.toList
                        |> List.map
                            (\( listName, listData ) ->
                                let
                                    isMovieListEmpty =
                                        List.isEmpty listData.listOfMovies

                                    someMoviesPosters =
                                        List.take 2 listData.listOfMovies |> List.map (\m -> m.posterPath)
                                in
                                Html.li []
                                    [ Html.div []
                                        [ Html.p [] [ text "List Name: " ]
                                        , Html.h3 [] [ text listName ]
                                        ]
                                    , if isMovieListEmpty then
                                        Html.div []
                                            [ Html.p [] [ text "You list is currently empty" ]
                                            , Html.a [ Attr.href "/search" ] [ text "add a movie" ]
                                            ]

                                      else
                                        let
                                            profilePartOfUrl =
                                                case model.id of
                                                    Just (Id id) ->
                                                        "/profile/" ++ id ++ "/list/" ++ listData.listId

                                                    Nothing ->
                                                        "/home"
                                        in
                                        Html.div []
                                            [ Html.ul []
                                                (someMoviesPosters |> List.map (\poster -> Html.li [] [ Html.img [ Attr.css [ Tw.w_12 ], Attr.src <| "https://image.tmdb.org/t/p/original" ++ poster ] [] ]))
                                            , Html.div []
                                                [ Html.p [] [ text "Shared with: " ]
                                                , Html.ul [] (listData.sharedWith |> Set.toList |> List.map (\friendsEmail -> Html.li [] [ text friendsEmail ]))
                                                ]
                                            , Html.a [ Attr.href <| profilePartOfUrl ] [ text "view list" ]
                                            ]
                                    ]
                            )
                    )
            ]
        ]
