module Main exposing (main)

import Browser
import Html exposing (Html, a, button, div, fieldset, form, h1, h6, hr, input, label, li, p, text, ul)
import Html.Attributes exposing (autofocus, class, for, href, placeholder, required, style, target, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Robot exposing (Grid, Robot)
import Svg exposing (defs, g, path, pattern, rect, svg)
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

                ( robot, historyEntry ) =
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
            { model | command = "", history = historyEntry :: model.history, robot = robot }


view : Model -> Html Msg
view model =
    div [ class "container", style "paddingTop" "6rem" ]
        [ div [ class "row" ]
            [ div [ class "column column-25" ] [ readmeView model ]
            , div [ class "column column-50", style "padding" "0 2rem" ]
                [ div [ class "row" ]
                    [ div [ class "column" ] [ formView model ] ]
                , div [ class "row" ]
                    [ div [ class "column" ] [ historyView model ] ]
                ]
            , div [ class "column column-25" ]
                [ div [ class "row" ]
                    [ div [ class "column" ]
                        [ gridView model ]
                    ]
                ]
            ]
        ]


readmeView : Model -> Html Msg
readmeView model =
    div []
        [ h1
            [ style "text-transform" "uppercase"
            , style "max-width" "min-content"
            , style "font-size" "3.5rem"
            ]
            [ text "Elm Toy Robot" ]
        , hr [ style "border-color" "#d1d1d1" ] []
        , h6 [ style "text-transform" "uppercase" ] [ text "Commands" ]
        , ul [ style "line-height" "1.6rem" ]
            [ li [] [ text "Place x, y, direction" ]
            , li [] [ text "Move (M)" ]
            , li [] [ text "Left (L)" ]
            , li [] [ text "Right (R)" ]
            , li [] [ text "Report" ]
            ]
        , hr [ style "border-color" "#d1d1d1" ] []
        , h6 [ style "text-transform" "uppercase" ] [ text "About" ]
        , p []
            [ text "A simple programming puzzle usually used as a part of the interview process. A good description "
            , a [ href "https://github.com/dctr/rea-robot/blob/master/PROBLEM.md", target "_blank" ] [ text "here" ]
            , text "."
            ]
        ]


formView : Model -> Html Msg
formView model =
    form [ onSubmit OnCommandSubmit ]
        [ fieldset []
            [ input
                [ type_ "text"
                , onInput OnCommandInput
                , placeholder "Enter a command: Place 3, 4, North"
                , required True
                , autofocus True
                , value model.command
                ]
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
        cellWidth =
            30

        cellWidthStr =
            String.fromInt cellWidth

        gridWidth =
            String.fromInt ((cellWidth * model.grid.width) + 1)

        center =
            ((toFloat cellWidth * toFloat model.grid.width) + 1) / 2

        rotate =
            "rotate(-90 " ++ String.fromFloat center ++ " " ++ String.fromFloat center ++ ")"
    in
    svg
        [ Attr.width gridWidth
        , Attr.height gridWidth
        , Attr.viewBox ("0 0 " ++ gridWidth ++ " " ++ gridWidth)
        , style "display" "block"
        , style "margin" "0 auto"
        ]
        [ defs []
            [ pattern
                [ Attr.id "grid"
                , Attr.width cellWidthStr
                , Attr.height cellWidthStr
                , Attr.patternUnits "userSpaceOnUse"
                ]
                [ path
                    [ Attr.d ("M " ++ cellWidthStr ++ " 0 L 0 0 0 " ++ cellWidthStr)
                    , Attr.fill "none"
                    , Attr.stroke "#333"
                    , Attr.strokeWidth "1"
                    ]
                    []
                ]
            ]
        , g [ Attr.transform rotate ]
            [ rect [ Attr.width "100%", Attr.height "100%", Attr.fill "url(#grid)" ] []
            , robotView model cellWidth
            ]
        ]


robotView : Model -> Int -> Html Msg
robotView model cellWidth =
    case model.robot of
        Robot.Unplaced ->
            Html.text ""

        Robot.Placed position direction ->
            let
                padding =
                    5

                -- bottom left point of cell
                -- Since the entire svg is rotated 90 degrees,
                -- the x and y positions are swapped
                cellOrigin =
                    { y = (position.x * cellWidth) + padding
                    , x = (position.y * cellWidth) + padding
                    }

                cellOriginStr =
                    { x = String.fromInt cellOrigin.x
                    , y = String.fromInt cellOrigin.y
                    }

                svgPath =
                    String.join " "
                        [ "M"
                        , cellOriginStr.x
                        , cellOriginStr.y
                        , "L"
                        , cellOriginStr.x
                        , String.fromInt (cellOrigin.y + 20)
                        , "L"
                        , String.fromInt (cellOrigin.x + 20)
                        , String.fromInt (cellOrigin.y + 10)
                        , "Z"
                        ]

                deg =
                    case direction of
                        Robot.North ->
                            "0"

                        Robot.East ->
                            "90"

                        Robot.South ->
                            "180"

                        Robot.West ->
                            "270"

                rotation =
                    "rotate(" ++ String.join " " [ deg, String.fromInt (cellOrigin.x + 10), String.fromInt (cellOrigin.y + 10) ] ++ ")"
            in
            path
                [ Attr.d svgPath
                , Attr.fill "LightBlue"
                , Attr.stroke "Blue"
                , Attr.strokeWidth "1"
                , Attr.transform rotation
                ]
                []
