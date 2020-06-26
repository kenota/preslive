module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Element
    exposing
        ( Attribute
        , Color
        , Element
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , htmlAttribute
        , layout
        , maximum
        , padding
        , px
        , rgb255
        , row
        , spacing
        , text
        , width
        , wrappedRow
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Json.Decode as Decode
import Json.Encode as Encode
import Lamdera exposing (ClientId)
import Task
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


primaryColor : Color
primaryColor =
    rgb255 89 195 205


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


identity : a -> a
identity a =
    a



{- Event handlers -}


pwChange : String -> FrontendMsg
pwChange s =
    AuthPwUpdated s


onEnter : msg -> Attribute msg
onEnter msg =
    htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    case url.path of
        "/admin" ->
            ( Connecting Admin
            , Task.succeed Connect |> Task.perform identity
            )

        _ ->
            ( Connecting User
            , Task.succeed Connect |> Task.perform identity
            )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            ( model, Cmd.none )

        UrlChanged url ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        Connect ->
            ( model, Cmd.none )

        QuestionToBackend q ->
            ( model, Lamdera.sendToBackend (AskQuestion q) )

        VoteToBackend vote ->
            ( model, Lamdera.sendToBackend (CastVote vote) )

        AuthPwUpdated newPassword ->
            case model of
                AdminAuth auth ->
                    ( AdminAuth (EnteringPassword newPassword), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Login ->
            case model of
                AdminAuth auth ->
                    case auth of
                        EnteringPassword pw ->
                            ( AdminAuth LoggingIn, Lamdera.sendToBackend (LoginAsAdmin pw) )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case model of
        Connecting mode ->
            case msg of
                State s ->
                    case mode of
                        Admin ->
                            ( AdminAuth (EnteringPassword ""), Cmd.none )

                        User ->
                            ( UserView s, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UserView currentState ->
            case msg of
                State newState ->
                    ( UserView newState, Cmd.none )

                LoginAsAdminReply reply ->
                    ( model, Cmd.none )

        AdminAuth auth ->
            case auth of
                LoggingIn ->
                    case msg of
                        LoginAsAdminReply reply ->
                            case reply of
                                Success adminState ->
                                    ( AdminLoggedIn adminState, Cmd.none )

                                Error s ->
                                    ( AdminAuth (ServerResponse s), Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        AdminLoggedIn _ ->
            ( model, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "Pres Live (powered by Lamdera)"
    , body =
        [ renderModelHtml model ]
    }


renderLobby : Int -> Html FrontendMsg
renderLobby joined =
    layout []
        (row [ width fill, spacing 30 ]
            [ column [ centerX, spacing 30, padding 30 ]
                [ el [ width fill, Font.center ] (text "Hello!")
                , el [ width fill, Font.center, Font.size 32 ] (text (String.fromInt joined ++ " Joined"))
                , el [ width fill, Font.center ] (text "We will being shortly, sit tight")
                ]
            ]
        )


renderLoginForm : String -> String -> Html FrontendMsg
renderLoginForm title pw =
    layout []
        (row [ centerX, centerY, width (fill |> maximum 300) ]
            [ column [ centerX, spacing 10 ]
                [ text title
                , Input.currentPassword
                    [ Input.focusedOnLoad
                    , onEnter Login
                    ]
                    { onChange = pwChange
                    , placeholder = Nothing
                    , label = Input.labelHidden "Password"
                    , text = pw
                    , show = False
                    }
                , Input.button
                    [ width fill
                    , padding 10
                    , Font.center
                    , Background.color primaryColor
                    ]
                    { onPress = Just Login
                    , label = text "Login"
                    }
                ]
            ]
        )


renderAuthState : AuthState -> Html FrontendMsg
renderAuthState authState =
    case authState of
        EnteringPassword pw ->
            renderLoginForm "" pw

        LoggingIn ->
            renderLoginForm "Logging in" ""

        ServerResponse resp ->
            renderLoginForm ("Error: " ++ resp) ""


shortPublicState : PresState -> Element msg
shortPublicState state =
    case state of
        Lobby joined ->
            el [] (text ("Lobby " ++ String.fromInt joined ++ " joined"))

        Asking q ->
            el [] (text "Asking question")


questionButton : String -> Element FrontendMsg
questionButton q =
    Input.button
        [ width fill, padding 10, Background.color primaryColor ]
        { onPress = Just (QuestionToBackend q), label = text q }


adminView : AdminState -> Html FrontendMsg
adminView model =
    let
        q =
            Dict.keys model.questions
    in
    layout []
        (column
            [ width (px 300)
            , centerX
            , height fill
            , padding 10
            , spacing 30
            ]
            [ row [] [ text "Current State: ", shortPublicState model.publicState ]
            , text "Ask question"
            , column [ spacing 10 ] (List.map questionButton q)
            ]
        )


countVote : String -> Dict String Int -> Dict String Int
countVote vote dict =
    case Dict.get vote dict of
        Nothing ->
            Dict.insert vote 1 dict

        Just nvotes ->
            Dict.insert vote (nvotes + 1) dict


singleVoteResult : String -> Int -> List (Element FrontendMsg) -> List (Element FrontendMsg)
singleVoteResult vote count lst =
    column [ spacing 5 ]
        [ el [] (text vote)
        , el [ width fill, Font.center ] (text (String.fromInt count))
        ]
        :: lst


voteResults : Dict ClientId String -> Element FrontendMsg
voteResults votes =
    let
        counts =
            List.foldl countVote Dict.empty (Dict.values votes)
    in
    wrappedRow [ centerX, spacing 15 ] (Dict.foldl singleVoteResult [] counts)


castVoteButton : String -> Element FrontendMsg
castVoteButton option =
    el []
        (Input.button
            [ Background.color (rgb255 192 192 192)
            , padding 15
            ]
            { label = text option, onPress = Just (VoteToBackend option) }
        )


renderQuestion : Question -> Html FrontendMsg
renderQuestion q =
    layout []
        (column
            [ width (px 480)
            , centerX
            , height fill
            , padding 10
            , spacing 20
            ]
            [ row [ width fill ] [ el [ width fill, Font.center ] (text q.question) ]
            , voteResults q.votes
            , wrappedRow [ spacing 5, centerX ] (List.map castVoteButton q.options)
            ]
        )


renderModelHtml : Model -> Html FrontendMsg
renderModelHtml m =
    case m of
        Connecting _ ->
            layout []
                (column
                    [ width (px 300)
                    , centerX
                    , height fill
                    , padding 10
                    ]
                    [ text "Connecting" ]
                )

        UserView state ->
            case state of
                Lobby n ->
                    renderLobby n

                Asking q ->
                    renderQuestion q

        AdminAuth s ->
            renderAuthState s

        AdminLoggedIn adminState ->
            adminView adminState
