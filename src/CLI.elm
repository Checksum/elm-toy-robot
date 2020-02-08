port module CLI exposing (main)

import Robot exposing (Grid, Robot)


main : Platform.Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    ()


type alias Model =
    { grid : Grid
    , robot : Robot
    }


type Msg
    = OnReceive String


port get : (String -> msg) -> Sub msg


port put : String -> Cmd msg


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        { grid, robot } =
            Robot.init ( 5, 5 )
    in
    ( { grid = grid
      , robot = robot
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    get OnReceive


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnReceive input ->
            let
                canonical =
                    String.toUpper (String.trim input)

                ( robot, response ) =
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
            ( { model | robot = robot }, put response )
