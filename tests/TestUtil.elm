module TestUtil exposing (..)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Set
import Test exposing (..)
import Types exposing (updatedMovieLists)


mockMovie1 =
    { id = 1
    , overview = "test overview 1"
    , posterPath = "/some/path"
    , releaseDate = "01/01/1970"
    , title = "Movie 1"
    }


mockMovie2 =
    { id = 2
    , overview = "test overview 2"
    , posterPath = "/some/path"
    , releaseDate = "01/01/1970"
    , title = "Movie 2"
    }


mockMovie3 =
    { id = 3
    , overview = "test overview 3"
    , posterPath = "/some/path"
    , releaseDate = "01/01/1970"
    , title = "Movie 3"
    }


mockListOfMovies =
    [ mockMovie1
    , mockMovie2
    ]


mockListOfMoviesWith3Items =
    [ mockMovie1
    , mockMovie2
    , mockMovie3
    ]


mockDict =
    Dict.fromList
        [ ( "MyList1"
          , { sharedWith = Set.empty
            , listOfMovies = mockListOfMovies
            , listId = "1"
            }
          )
        , ( "MyList2"
          , { sharedWith = Set.empty
            , listOfMovies = mockListOfMovies
            , listId = "2"
            }
          )
        , ( "MyList3"
          , { sharedWith = Set.empty
            , listOfMovies = mockListOfMovies
            , listId = "3"
            }
          )
        ]


mockDictWithDuplicate =
    Dict.fromList
        [ ( "MyList1"
          , { sharedWith = Set.empty
            , listOfMovies = mockListOfMovies
            , listId = "1"
            }
          )
        , ( "MyList2"
          , { sharedWith = Set.empty
            , listOfMovies = mockListOfMovies
            , listId = "2"
            }
          )
        , ( "MyList3"
          , { sharedWith = Set.empty
            , listOfMovies = expectedListOfMovies
            , listId = "3"
            }
          )
        ]


mockListOfTargetListIds =
    [ "2" ]


mockListOfTargetListIdsMultiple =
    mockListOfTargetListIds ++ [ "3" ]


expectedListOfMovies =
    mockListOfMoviesWith3Items


expectedDict =
    Dict.fromList
        [ ( "MyList1"
          , { sharedWith = Set.empty
            , listOfMovies = mockListOfMovies
            , listId = "1"
            }
          )
        , ( "MyList2"
          , { sharedWith = Set.empty
            , listOfMovies = expectedListOfMovies
            , listId = "2"
            }
          )
        , ( "MyList3"
          , { sharedWith = Set.empty
            , listOfMovies = mockListOfMovies
            , listId = "3"
            }
          )
        ]


expectedDictMultiple =
    Dict.fromList
        [ ( "MyList1"
          , { sharedWith = Set.empty
            , listOfMovies = mockListOfMovies
            , listId = "1"
            }
          )
        , ( "MyList2"
          , { sharedWith = Set.empty
            , listOfMovies = expectedListOfMovies
            , listId = "2"
            }
          )
        , ( "MyList3"
          , { sharedWith = Set.empty
            , listOfMovies = expectedListOfMovies
            , listId = "3"
            }
          )
        ]


updateMovieListTest : Test
updateMovieListTest =
    describe "updatedMovieLists"
        [ test "should add movie if not duplicate" <|
            \_ ->
                updatedMovieLists mockDict mockMovie3 mockListOfTargetListIds
                    |> Expect.equal
                        expectedDict
        , test "should not add movie if duplicate" <|
            \_ ->
                updatedMovieLists mockDict mockMovie2 mockListOfTargetListIds
                    |> Expect.equal
                        mockDict
        , test "should add in multiple lists if not duplicate" <|
            \_ ->
                updatedMovieLists mockDict mockMovie3 mockListOfTargetListIdsMultiple
                    |> Expect.equal
                        expectedDictMultiple
        , test "should try to add same movie in 2 lists but succeed only in one where its not duplicate" <|
            \_ ->
                updatedMovieLists mockDict mockMovie3 mockListOfTargetListIdsMultiple
                    |> Expect.equal
                        expectedDictMultiple
        ]
