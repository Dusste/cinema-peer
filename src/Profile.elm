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


type alias Model =
    ProfileModel


initModel : Model
initModel =
    { name = "", movieLists = Dict.empty }


init : User -> ( Model, Cmd Types.ProfileMsg )
init { name, movieLists } =
    ( { name = name, movieLists = movieLists }, Cmd.none )


update : Types.ProfileMsg -> Model -> ( Model, Cmd Types.ProfileMsg )
update msg model =
    case msg of
        StoreName nameValue ->
            ( { model | name = nameValue }, Cmd.none )

        -- ResponseUserUpdate { name } ->
        --     let
        --         _ =
        --             Debug.log "usao u ResponseUserUpdate in PRfile " name
        --     in
        -- ( { model | nameValue = name }, Cmd.none )
        SubmitName ->
            ( model, sendToBackend <| RequestUpdateName model.name )



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
            , Html.ul
                []
                (model.movieLists
                    |> Dict.toList
                    |> List.map
                        (\( listName, maybeListData ) ->
                            Html.li []
                                [ Html.div []
                                    [ Html.p [] [ text "List Name: " ]
                                    , Html.h3 [] [ text listName ]
                                    ]
                                , case maybeListData of
                                    Just movieData ->
                                        Html.div []
                                            [ Html.div []
                                                [ Html.p [] [ text "Shared with: " ]
                                                , Html.ul [] (movieData.sharedWith |> Set.toList |> List.map (\friendsEmail -> Html.li [] [ text friendsEmail ]))
                                                ]
                                            , Html.div []
                                                [ Html.p [] [ text "Movies: " ]
                                                , Html.ul [] (movieData.movieList |> List.map (\{ title } -> Html.li [] [ text title ]))
                                                ]
                                            ]

                                    Nothing ->
                                        text ""
                                ]
                        )
                )
            ]
        ]
