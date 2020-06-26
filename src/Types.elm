module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import Set exposing (Set)
import Url exposing (Url)



{-
   Pres.live is about asking questions from the people going to
   elm meetup.

   There are several questions usually:

   * Your level of programming experience
   * Your level of elm experience
   * How are you feeling
   * Where are you from

   The purpose of this project is to play around with elm, lamdera and
   also do something useful.

-}
{-
   So, what question look like?
-}
-- Question is a string of question itself with different answers


type alias Question =
    { question : String
    , options : List String
    , votes : Dict ClientId String
    }



{-
   Now, if we think about the state of the presentation, what do we have?
-}


type PresState
    = Lobby Int
    | Asking Question



{-
   What do we know about frontend state?

   1. Globally it can be one of two ways: "Admin" or "User"
   2. For each of those states we can have Connecting or Connected model
   3. Admin user also need to login
-}


type Mode
    = User
    | Admin


type AuthState
    = EnteringPassword String
    | LoggingIn
    | ServerResponse String


type FrontendModel
    = Connecting Mode
    | UserView PresState
    | AdminAuth AuthState
    | AdminLoggedIn AdminState


type alias BackendModel =
    { question : Maybe Question
    , clients : Set ClientId
    , admins : Set ClientId
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | Connect
    | NoOpFrontendMsg
    | AuthPwUpdated String
    | Login
    | QuestionToBackend String
    | VoteToBackend String


type ToBackend
    = Hello
    | LoginAsAdmin String
    | AskQuestion String
    | CastVote String


type BackendMsg
    = NoOpBackendMsg
    | ClientConnect SessionId ClientId
    | ClientDisconnect SessionId ClientId


type alias AdminState =
    { questions : Dict String (List String)
    , publicState : PresState
    }


type AdminLoginReplyData
    = Success AdminState
    | Error String


type ToFrontend
    = State PresState
    | LoginAsAdminReply AdminLoginReplyData
