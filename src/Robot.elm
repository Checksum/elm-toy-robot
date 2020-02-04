module Robot exposing (Grid(..), Msg(..), Robot(..), State, init, update)

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


command : Regex.Regex
command =
    Maybe.withDefault Regex.never <|
        Regex.fromStringWith { caseInsensitive = True, multiline = False } <|
            "^((PLACE)\\s+(\\d+)\\s*,\\s*(\\d+)\\s*,\\s*(NORTH|SOUTH|EAST|WEST)|LEFT|RIGHT|MOVE|REPORT)$"


parseCommand : String -> Command
parseCommand cmdString =
    let
        match =
            Regex.find command cmdString
                |> List.map .submatches
                |> List.map (List.filterMap identity)
                |> List.head
                |> Maybe.withDefault []
    in
    case match of
        cmd :: rest ->
            case cmd of
                "RIGHT" ->
                    Rotate Right

                "LEFT" ->
                    Rotate Left

                "MOVE" ->
                    Move

                "REPORT" ->
                    Report

                "PLACE" ->
                    placeFromRegexMatch rest

                _ ->
                    UnknownCommand

        _ ->
            UnknownCommand


placeFromRegexMatch : List String -> Command
placeFromRegexMatch params =
    case params of
        p1 :: p2 :: dir :: [] ->
            let
                maybePoint =
                    parsePoint p1 p2

                maybeDirection =
                    parseDirection dir
            in
            case ( maybePoint, maybeDirection ) of
                ( Just point, Just direction ) ->
                    Place point direction

                _ ->
                    UnknownCommand

        _ ->
            UnknownCommand


type alias State =
    { grid : Grid
    , robot : Robot
    }


init : ( Int, Int ) -> State
init ( width, height ) =
    { grid = Grid ( width, height ), robot = Unplaced }


type Msg
    = Dispatch String


update : Command -> State -> State
update cmd state =
    state
