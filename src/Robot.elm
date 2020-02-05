module Robot exposing (Command(..), Grid(..), Msg(..), Robot(..), State, init, parseCommand, update)

import Parser as P exposing ((|.), (|=))
import Regex


type alias Point =
    { x : Int
    , y : Int
    }


parsePoint : String -> String -> Maybe Point
parsePoint xStr yStr =
    let
        xInt =
            String.toInt xStr

        yInt =
            String.toInt yStr
    in
    case ( xInt, yInt ) of
        ( Just x, Just y ) ->
            Just (Point x y)

        _ ->
            Nothing


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


parseDirection : String -> Maybe Direction
parseDirection str =
    case String.toUpper str of
        "EAST" ->
            Just East

        "WEST" ->
            Just West

        "NORTH" ->
            Just North

        "SOUTH" ->
            Just South

        _ ->
            Nothing


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


type Grid
    = Grid ( Int, Int )


type Robot
    = Placed Point Direction
    | Unplaced


rotate : Rotation -> Robot -> Robot
rotate rotation robot =
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


move : Robot -> Robot
move robot =
    case robot of
        Placed position direction ->
            let
                { moveStep } =
                    directionMap direction
            in
            Placed (moveStep position) direction

        Unplaced ->
            robot


report : Robot -> String
report robot =
    case robot of
        Placed point direction ->
            "Robot at " ++ pointToString (Just point) ++ " facing " ++ directionToString (Just direction)

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
            |. P.keyword "MOVE"

        -- Report
        , P.succeed Report
            |. P.keyword "REPORT"

        -- Rotate
        , P.succeed Rotate
            |= P.oneOf
                [ P.succeed Left
                    |. P.keyword "LEFT"
                , P.succeed Right
                    |. P.keyword "RIGHT"
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


type alias State =
    { grid : Grid
    , robot : Robot
    }


init : ( Int, Int ) -> State
init ( width, height ) =
    { grid = Grid ( width, height ), robot = Unplaced }


type Msg
    = Dispatch String


update : Command -> { a | grid : Grid, robot : Robot } -> State
update cmd { grid, robot } =
    { grid = grid, robot = robot }
