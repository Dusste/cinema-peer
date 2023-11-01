module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Email.Html
import Email.Html.Attributes
import EmailAddress exposing (EmailAddress)
import Env
import Http
import Lamdera exposing (ClientId, SessionId)
import List.Nonempty
import SendGrid
import Set exposing (Set)
import Sha256
import String.Nonempty exposing (NonemptyString(..))
import Time
import Url exposing (Url)


type alias SendEmailConfig =
    { subject : NonemptyString
    , toEmail : List.Nonempty.Nonempty EmailAddress
    , fromEmail : EmailAddress
    , content : Email.Html.Html
    , apiKey : SendGrid.ApiKey
    , toMsg : Result SendGrid.Error () -> BackendMsg
    }


type Session
    = IsLoading
    | Anonymus
    | LoggedIn User


type Route
    = HomeRoute
    | ProfileRoute UserId
    | LoginRoute
    | SearchRoute
    | LoginTokenRoute (Maybe Token)
    | NotFoundRoute


type UserId
    = UserId String


type Token
    = Token String


type ResultsState
    = Initial
    | Loading
    | Success (List Movie)


type alias ProfileModel =
    { name : String
    , movieLists : Dict MovieListName (Maybe MovieListData)
    }


type alias LoginModel =
    { emailValue : String, error : Maybe String, notification : Maybe String }


type alias SearchModel =
    { resultsState : ResultsState
    , movieValue : String
    , timer : Timer
    }


type PageMsgs
    = ProfileMsg ProfileMsg
    | SearchMsg SearchMsg


type Page
    = LoginPage LoginModel
    | ProfilePage ProfileModel
    | SearchPage SearchModel
    | HomePage
    | NotFoundPage


type alias Movie =
    { id : Int
    , overview : String
    , posterPath : String
    , releaseDate : String
    , title : String
    }


type alias User =
    { id : Id
    , name : String
    , email : Email
    , movieLists : Dict MovieListName (Maybe MovieListData)
    }


type alias MovieListData =
    { sharedWith : Set String --  friends Email ?
    , movieList : List Movie
    , listId : Int
    }


type alias MovieListName =
    String


type alias LoginTokenData =
    { createTime : Time.Posix, email : Email }


type Timer
    = TimerOn Int
    | TimerOff


type Email
    = Email String


type Id
    = Id String


type alias FrontendModel =
    { key : Key
    , url : Url
    , sessionStatus : Session
    , error : Maybe String
    , page : Page
    , showCreateListModal : Bool
    , newListName : String
    , notifications : List String
    }


type alias BackendModel =
    { users : Dict SessionId User
    , connections : Dict SessionId SessionId -- TODO what should connection contain ? Mybe it can be Set of Strings
    , currentTime : Time.Posix
    , pendingAuth : Dict String LoginTokenData -- users that started login journey and got email with token
    }


type LoginMsg
    = NoOp
    | StoreEmail String
    | TryRequestLogin String


type ProfileMsg
    = StoreName String
    | SubmitName



-- | ResponseUserUpdate User


type SearchMsg
    = StoreMovie String
    | Tick Time.Posix
    | ResponseFetchMovies (Result String (List Movie))


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | GotLoginMsg LoginMsg
    | GotProfileMsg ProfileMsg
    | GotSearchMsg SearchMsg
    | TriggerLogout
    | HideNotification
    | OpenNewListModal
    | StoreNewListName String
    | CreateNewList


type ToBackend
    = NoOpToBackend
    | RequestFetchMovies String
    | RequestLogin Email
    | RequestAuth Token
    | RequestLogout
    | RequestUpdateName String
    | RequestNewList String
    | GotSession


type BackendMsg
    = NoOpBackendMsg
    | GotMovies (Result Http.Error ( SessionId, List Movie ))
    | LoginTokenSend (Result SendGrid.Error ())
    | GetTime Time.Posix
    | UserConnected SessionId ClientId
    | UserDisconnected SessionId ClientId


type ToFrontend
    = ResponseAuth Session String
    | UpdateToPages PageMsgs
    | GoHome


getLoginToken : Time.Posix -> Token
getLoginToken currentTime =
    Env.salt
        ++ String.fromInt (Time.posixToMillis currentTime)
        |> Sha256.sha256
        |> Token


getId : Time.Posix -> Id
getId currentTime =
    Env.salt
        ++ String.fromInt (Time.posixToMillis currentTime)
        |> Sha256.sha256
        |> String.left 5
        |> Id


sendEmailToUser : SendEmailConfig -> Cmd BackendMsg
sendEmailToUser { subject, toEmail, fromEmail, content, apiKey, toMsg } =
    SendGrid.htmlEmail
        { subject = subject

        -- subject = NonemptyString 'S' "ubject"
        , to = toEmail

        -- , to = List.Nonempty.fromElement recipient
        , content = content

        -- , content =
        --     Email.Html.div
        --         []
        --         [ Email.Html.text "Hi!"
        --         , Email.Html.text "Welcome to Cinema Peer !"
        --         , Email.Html.a [ Email.Html.Attributes.href "" ] [ Email.Html.text "click on a link to login" ]
        --         ]
        , nameOfSender = "Cinema-Peer"

        -- , emailAddressOfSender =
        --     -- this-can-be-anything@test.com
        --     { localPart = "no-reply"
        --     , tags = []
        --     , domain = "test"
        --     , tld = [ "com" ]
        --     }
        , emailAddressOfSender = fromEmail
        }
        |> SendGrid.sendEmail toMsg apiKey


insertPendingAuth : Token -> BackendModel -> Email -> Dict String LoginTokenData
insertPendingAuth (Token token) model email =
    Dict.insert token { createTime = model.currentTime, email = email } model.pendingAuth


getConfig : EmailAddress -> EmailAddress -> Token -> SendEmailConfig
getConfig toEmail fromEmail tkn =
    { subject = NonemptyString 'C' "inema-Peer login"
    , toEmail = List.Nonempty.fromElement toEmail
    , fromEmail = fromEmail
    , content =
        Email.Html.div
            []
            [ Email.Html.div [] [ Email.Html.text "Hi!" ]
            , Email.Html.div [] [ Email.Html.text "Welcome to Cinema Peer !" ]
            , Email.Html.a [ Email.Html.Attributes.href <| getLinkToLogin tkn ] [ Email.Html.text "click on a link to login" ]
            ]
    , apiKey = SendGrid.apiKey Env.sendGridApiKey
    , toMsg = LoginTokenSend
    }


getLinkToLogin : Token -> String
getLinkToLogin (Token loginToken) =
    case Env.mode of
        Env.Production ->
            "https://cinema-peer.lamdera.app/loginToken?token=" ++ loginToken

        Env.Development ->
            "http://localhost:" ++ Env.localhost ++ "/loginToken?token=" ++ loginToken


tokenToString : Token -> String
tokenToString (Token token) =
    token
