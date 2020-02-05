module Main exposing (main)

import Browser
import Html exposing (Html, button, div, fieldset, form, input, label, p, text)
import Html.Attributes exposing (class, for, placeholder, required, style, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Robot exposing (Command(..), Grid(..), Robot(..))


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias AppState state =
    { state
        | command : String
        , history : List String
    }


type alias Model =
    AppState Robot.State


type Msg
    = OnCommandInput String
    | OnCommandSubmit
    | Noop


init : Model
init =
    let
        { grid, robot } =
            Robot.init ( 5, 5 )
    in
    { grid = grid
    , robot = robot
    , command = ""
    , history = []
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnCommandInput str ->
            { model | command = str }

        OnCommandSubmit ->
            processCommand model

        _ ->
            model


processCommand : Model -> Model
processCommand model =
    let
        canonical =
            String.toUpper <| String.trim model.command

        command =
            Robot.parseCommand canonical

        historyEntry =
            case command of
                UnknownCommand ->
                    "Unknown or invalid command: " ++ canonical

                _ ->
                    canonical
    in
    { model | history = historyEntry :: model.history, command = "" }


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "column column-50 column-offset-25" ]
                [ commandForm model
                ]
            ]
        ]


commandForm : Model -> Html Msg
commandForm model =
    form [ onSubmit OnCommandSubmit ]
        [ fieldset []
            [ input
                [ type_ "text", onInput OnCommandInput, placeholder "Place 3, 4, North", required True, value model.command ]
                []
            , button [ type_ "submit", style "display" "none" ] [ text "Submit" ]
            ]
        ]
