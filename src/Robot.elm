module Robot exposing (Command(..), Direction(..), Grid, Robot(..), RobotResult(..), init, run, update)

import Parser as P exposing ((|.), (|=))


type alias Point =
    { x : Int
    , y : Int
    }


pointToString : Maybe Point -> String
pointToString point =
    case point of
        Just { x, y } ->
            "(" ++ String.fromInt x ++ "," ++ String.fromInt y ++ ")"

        Nothing ->
            "Invalid point"


type Direction
    = East
    | West
    | North
    | South


directionToString : Maybe Direction -> String
directionToString direction =
    case direction of
        Just dir ->
            case dir of
                East ->
                    "EAST"

                West ->
                    "WEST"

                North ->
                    "NORTH"

                South ->
                    "SOUTH"

        Nothing ->
            "Unknown Direction"



-- Direction transformation
-- How to transform when the rover turns left, turns right, and moves


type alias DirectionMapping =
    { turnLeft : Direction
    , turnRight : Direction
    , moveStep : Point -> Point
    }


directionMap : Direction -> DirectionMapping
directionMap direction =
    let
        ( turnLeft, turnRight, moveStep ) =
            case direction of
                North ->
                    ( West, East, \pt -> { pt | y = pt.y + 1 } )

                South ->
                    ( East, West, \pt -> { pt | y = pt.y - 1 } )

                East ->
                    ( North, South, \pt -> { pt | x = pt.x + 1 } )

                West ->
                    ( South, North, \pt -> { pt | x = pt.x - 1 } )
    in
    DirectionMapping turnLeft turnRight moveStep


type Rotation
    = Left
    | Right


type Command
    = Place Point Direction
    | Move
    | Rotate Rotation
    | Report
    | UnknownCommand


type alias Grid =
    { width : Int
    , height : Int
    }


type Robot
    = Placed Point Direction
    | Unplaced


place : Grid -> Robot -> Point -> Direction -> Robot
place grid robot position direction =
    case robot of
        Unplaced ->
            if position.x < grid.width && position.y < grid.height then
                Placed position direction

            else
                robot

        _ ->
            robot


rotate : Grid -> Robot -> Rotation -> Robot
rotate grid robot rotation =
    case robot of
        Placed position direction ->
            let
                { turnLeft, turnRight } =
                    directionMap direction

                newDirection =
                    case rotation of
                        Left ->
                            turnLeft

                        Right ->
                            turnRight
            in
            Placed position newDirection

        Unplaced ->
            robot


move : Grid -> Robot -> Robot
move grid robot =
    case robot of
        Placed position direction ->
            let
                { moveStep } =
                    directionMap direction

                newPosition =
                    moveStep position

                x =
                    clamp 0 (grid.width - 1) newPosition.x

                y =
                    clamp 0 (grid.height - 1) newPosition.y
            in
            Placed { x = x, y = y } direction

        Unplaced ->
            robot


report : Grid -> Robot -> String
report grid robot =
    case robot of
        Placed position direction ->
            "Robot at " ++ pointToString (Just position) ++ " facing " ++ directionToString (Just direction)

        Unplaced ->
            "Robot not placed yet"


parseCommand : String -> Command
parseCommand str =
    case P.run commandParser str of
        Ok command ->
            command

        Err _ ->
            UnknownCommand


commandParser : P.Parser Command
commandParser =
    P.oneOf
        [ -- Move
          P.succeed Move
            |. P.oneOf
                [ P.keyword "MOVE"
                , P.keyword "M"
                ]

        -- Report
        , P.succeed Report
            |. P.keyword "REPORT"

        -- Rotate
        , P.succeed Rotate
            |= P.oneOf
                [ P.succeed Left
                    |. P.oneOf
                        [ P.keyword "LEFT"
                        , P.keyword "L"
                        ]
                , P.succeed Right
                    |. P.oneOf
                        [ P.keyword "RIGHT"
                        , P.keyword "R"
                        ]
                ]

        -- Place
        , P.succeed Place
            |. P.keyword "PLACE"
            |. P.spaces
            |= P.oneOf
                [ P.succeed Point
                    |= P.int
                    |. P.spaces
                    |. P.symbol ","
                    |. P.spaces
                    |= P.int
                ]
            |. P.spaces
            |. P.symbol ","
            |. P.spaces
            |= P.oneOf
                [ P.succeed North
                    |. P.keyword "NORTH"
                , P.succeed South
                    |. P.keyword "SOUTH"
                , P.succeed East
                    |. P.keyword "EAST"
                , P.succeed West
                    |. P.keyword "WEST"
                ]

        -- Unknown
        , P.succeed UnknownCommand
        ]


type RobotResult
    = UpdatedRobot Robot
    | RobotReport String


init : ( Int, Int ) -> { grid : Grid, robot : Robot }
init ( width, height ) =
    { grid = { width = width, height = height }, robot = Unplaced }


update : Command -> { a | grid : Grid, robot : Robot } -> RobotResult
update cmd { grid, robot } =
    case robot of
        Placed _ _ ->
            case cmd of
                Rotate rotation ->
                    UpdatedRobot (rotate grid robot rotation)

                Move ->
                    UpdatedRobot (move grid robot)

                Report ->
                    RobotReport (report grid robot)

                _ ->
                    UpdatedRobot robot

        Unplaced ->
            case cmd of
                Place point direction ->
                    UpdatedRobot (place grid robot point direction)

                _ ->
                    RobotReport (report grid robot)


run : { a | grid : Grid, robot : Robot } -> String -> Result String RobotResult
run { grid, robot } input =
    let
        command =
            parseCommand input
    in
    case command of
        UnknownCommand ->
            Err ("Unknown or invalid command: " ++ input)

        _ ->
            Ok (update command { robot = robot, grid = grid })
