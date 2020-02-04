module Main exposing (main)

import Browser
import Html exposing (Html, button, div, form, input, p, text)
import Html.Attributes exposing (type_)
import Html.Events exposing (onInput, onSubmit)
import Robot exposing (Grid(..), Robot(..))


main : Program () AppState Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model state =
    { state
        | name : String
    }


type alias AppState =
    Model Robot.State


type Msg
    = OnCommandTyping String
    | OnCommandSubmit
    | Noop


init : AppState
init =
    let
        { grid, robot } =
            Robot.init ( 5, 5 )
    in
    { grid = grid, robot = robot, name = "Foo" }


update : Msg -> AppState -> AppState
update msg model =
    model


view : AppState -> Html Msg
view model =
    div []
        [ form [ onSubmit OnCommandSubmit ]
            [ input [ onInput OnCommandTyping ] []
            , button [ type_ "submit" ] [ text "Submit" ]
            ]
        ]
