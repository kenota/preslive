module Backend exposing (..)

import Debug
import Dict exposing (Dict)
import Html
import Lamdera exposing (ClientId, SessionId, onConnect, onDisconnect)
import Set exposing (Set)
import Types exposing (..)


questionaire : Dict String (List String)
questionaire =
    Dict.fromList
        [ ( "How are you feeling?", [ "👍", "👎" ] )
        , ( "Your programming level", [ "Beginner", "Confident", "Professional" ] )
        , ( "Your programming level in ELM", [ "Beginner", "Confident", "Professional" ] )
        , ( "What are you using elm for?", [ "Hobby", "Work", "Microwave" ] )
        , ( "Where are you from?", [ "Earth", "Deep Space" ] )
        ]


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub BackendMsg
subscriptions model =
    Sub.batch
        [ Lamdera.onConnect ClientConnect
        , Lamdera.onDisconnect ClientDisconnect
        ]


init : ( Model, Cmd BackendMsg )
init =
    ( { question = Nothing
      , clients = Set.empty
      , admins = Set.empty
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        ClientConnect _ clientId ->
            let
                newModel =
                    { model | clients = Set.insert clientId model.clients }
            in
            ( newModel, broadcastPublicState newModel )

        ClientDisconnect _ clientId ->
            let
                newModel =
                    { model
                        | clients = Set.remove clientId model.clients
                        , admins = Set.remove clientId model.admins
                    }
            in
            ( newModel, broadcastPublicState newModel )


publicState : BackendModel -> PresState
publicState m =
    case m.question of
        Nothing ->
            Lobby (Set.size m.clients)

        Just q ->
            Asking q


broadcastPublicState : BackendModel -> Cmd BackendMsg
broadcastPublicState m =
    Lamdera.broadcast (State (publicState m))


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        Hello ->
            ( model, Cmd.none )

        CastVote answer ->
            case model.question of
                Just q ->
                    let
                        newModel =
                            if List.member answer q.options then
                                let
                                    newQ =
                                        { q | votes = Dict.insert clientId answer q.votes }
                                in
                                { model | question = Just newQ }

                            else
                                model
                    in
                    ( newModel, broadcastPublicState newModel )

                Nothing ->
                    ( model, Cmd.none )

        AskQuestion s ->
            case Set.member clientId model.admins of
                True ->
                    case Dict.get s questionaire of
                        Nothing ->
                            ( model, Cmd.none )

                        Just opt ->
                            let
                                newModel =
                                    { model
                                        | question =
                                            Just
                                                { question = s, options = opt, votes = Dict.empty }
                                    }
                            in
                            ( newModel, broadcastPublicState newModel )

                False ->
                    ( model, Cmd.none )

        LoginAsAdmin pw ->
            case pw of
                "lamdera" ->
                    let
                        newModel =
                            { model | admins = Set.insert clientId model.admins }
                    in
                    ( newModel
                    , Lamdera.sendToFrontend clientId
                        (LoginAsAdminReply (Success { publicState = publicState newModel, questions = questionaire }))
                    )

                _ ->
                    ( model, Lamdera.sendToFrontend clientId (LoginAsAdminReply (Error "Wrong password")) )
