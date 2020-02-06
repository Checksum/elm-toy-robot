module Main exposing (main)

import Browser
import Html exposing (Html, button, div, fieldset, form, input, label, p, text)
import Html.Attributes exposing (autofocus, class, for, placeholder, required, style, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Robot exposing (Grid, Robot)
import Svg exposing (defs, path, pattern, rect, svg)
import Svg.Attributes as Attr


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    { grid : Grid
    , robot : Robot
    , command : String
    , history : List String
    }


type Msg
    = OnCommandInput String
    | OnCommandSubmit


init : Model
init =
    let
        { grid, robot } =
            Robot.init ( 5, 5 )
    in
    { grid = grid
    , robot = robot
    , command = "place 0,0,north"
    , history = []
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnCommandInput str ->
            { model | command = str }

        OnCommandSubmit ->
            let
                canonical =
                    String.toUpper (String.trim model.command)

                ( robot, history ) =
                    case Robot.run model canonical of
                        Ok result ->
                            case result of
                                Robot.UpdatedRobot newRobot ->
                                    ( newRobot, canonical )

                                Robot.RobotReport report ->
                                    ( model.robot, report )

                        Err error ->
                            ( model.robot, error )
            in
            { model | command = "", history = history :: model.history, robot = robot }


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "column column-50 column-offset-25" ]
                [ formView model
                , div [ class "clearfix" ]
                    [ div [ class "float-left" ] [ historyView model ]
                    , div [ class "float-right" ] [ gridView model ]
                    ]
                ]
            ]
        ]


formView : Model -> Html Msg
formView model =
    form [ onSubmit OnCommandSubmit ]
        [ fieldset []
            [ input
                [ type_ "text", onInput OnCommandInput, placeholder "Place 3, 4, North", required True, autofocus True, value model.command ]
                []
            , button [ type_ "submit", style "display" "none" ] [ text "Submit" ]
            ]
        ]


historyView : Model -> Html Msg
historyView model =
    div []
        (model.history
            |> List.map (\entry -> p [ style "margin" "0 5px" ] [ text entry ])
        )



-- https://stackoverflow.com/questions/22013281/drawing-a-grid-using-svg-markup


gridView : Model -> Html Msg
gridView model =
    let
        width =
            30

        cell =
            String.fromInt width

        gridWidth =
            String.fromInt ((width * model.grid.width) + 1)
    in
    svg [ Attr.width gridWidth, Attr.height gridWidth ]
        [ defs []
            [ pattern [ Attr.id "grid", Attr.width cell, Attr.height cell, Attr.patternUnits "userSpaceOnUse" ]
                [ path [ Attr.d ("M " ++ cell ++ " 0 L 0 0 0 " ++ cell), Attr.fill "none", Attr.stroke "#333", Attr.strokeWidth "1" ] []
                ]
            ]
        , rect [ Attr.width "100%", Attr.height "100%", Attr.fill "url(#grid)" ] []
        , robotView model width
        ]


robotView : Model -> Int -> Html Msg
robotView model cell =
    case model.robot of
        Robot.Unplaced ->
            Html.text ""

        Robot.Placed position direction ->
            let
                gridW =
                    model.grid.width

                gridH =
                    model.grid.height

                startX =
                    5

                startY =
                    gridH * cell - 5

                svgPath =
                    "M " ++ String.fromInt startX ++ " " ++ String.fromInt startY ++ " L " ++ String.fromInt startX ++ " " ++ String.fromInt (startY - 20) ++ " L 25 " ++ String.fromInt (startY - 10) ++ " Z"

                x =
                    String.fromInt (clamp 0 ((gridW - 1) * cell) ((position.x - 1) * cell))

                y =
                    String.fromInt ((position.y - 1) * cell)

                translate =
                    "translate(0 " ++ x ++ ")"

                deg =
                    case direction of
                        Robot.East ->
                            "0"

                        Robot.South ->
                            "90"

                        Robot.West ->
                            "180"

                        Robot.North ->
                            "270"

                rotation =
                    "rotate(" ++ deg ++ " " ++ String.fromInt (cell // 2) ++ " " ++ String.fromInt (startY - 10) ++ ")"
            in
            path
                [ Attr.d svgPath
                , Attr.fill "LightBlue"
                , Attr.stroke "Blue"
                , Attr.strokeWidth "1"
                , Attr.transform (rotation ++ translate)

                -- , Attr.transform ("rotate(" ++ rotation ++ " 15 15)" ++ " translate(" ++ x ++ " " ++ y ++ ")")
                ]
                []
