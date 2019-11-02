module Ship exposing (Orientation(..), Position, Ship, Type(..), randomizeOrientation, randomizePositions)

import List.Extra
import Random
import Random.Extra
import Random.List


type alias Ship =
    { class : Type
    , size : Int
    , position : Position
    , orientation : Orientation
    }


type alias Position =
    { x : Int, y : Int }


type Type
    = Destroyer
    | Submarine
    | Cruiser
    | Battleship
    | Carrier


type Orientation
    = Horizontal
    | Vertical


takenPos =
    Position -1 -1


boundaryPos =
    Position -2 -2


overlapPos =
    Position -3 -3


randomizeOrientation : List Ship -> Random.Generator (List Orientation)
randomizeOrientation ships =
    Random.map
        (List.map mapToOrientation)
        (Random.list (List.length ships) (Random.int 0 1))


mapToOrientation : Int -> Orientation
mapToOrientation n =
    if n == 0 then
        Horizontal

    else
        Vertical


randomizePositions : List Ship -> List (List Position) -> Random.Generator (List Position)
randomizePositions ships positions =
    let
        maybeShip =
            List.head ships

        cols =
            List.head positions
                |> Maybe.map List.length
                |> Maybe.withDefault 0

        rows =
            List.length positions

        availablePositions =
            List.concat positions
    in
    case maybeShip of
        Nothing ->
            Random.constant availablePositions

        Just ship ->
            Random.List.choose (choosePosition cols rows ship availablePositions)
                |> Random.andThen
                    (\posAndList ->
                        let
                            maybePos =
                                Tuple.first posAndList
                        in
                        case maybePos of
                            Nothing ->
                                Random.constant [ takenPos ]

                            Just pos ->
                                if List.length ships == 0 then
                                    Random.constant [ pos ]

                                else
                                    Random.map
                                        ((::) pos)
                                        (randomizePositions
                                            (List.drop 1 ships)
                                            (List.Extra.groupsOf
                                                cols
                                                (placeShipPosition cols ship pos availablePositions)
                                            )
                                        )
                    )


choosePosition : Int -> Int -> Ship -> List Position -> List Position
choosePosition cols rows ship positions =
    List.filter
        (\n -> n.x > -1)
        (placeBoundaries
            cols
            rows
            ship
            (placeOverlapPositions cols ship positions)
        )


placeBoundaries : Int -> Int -> Ship -> List Position -> List Position
placeBoundaries cols rows ship positions =
    case ship.orientation of
        Horizontal ->
            List.Extra.updateIfIndex
                (byHorizontal cols rows ship.size)
                (always boundaryPos)
                positions

        Vertical ->
            List.Extra.updateIfIndex
                (byVertical rows ship.size)
                (always boundaryPos)
                positions


byHorizontal : Int -> Int -> Int -> Int -> Bool
byHorizontal cols rows size index =
    (rows * cols) - (size - 1) * cols - 1 < index


byVertical : Int -> Int -> Int -> Bool
byVertical rows size index =
    (rows - modBy 10 index) < size


placeOverlapPositions : Int -> Ship -> List Position -> List Position
placeOverlapPositions cols ship positions =
    let
        indices =
            List.Extra.elemIndices takenPos positions
    in
    case ship.orientation of
        Horizontal ->
            List.Extra.updateIfIndex
                (\idx ->
                    List.member
                        idx
                        (horizontalOverlapIndices cols ship.size indices)
                )
                (always overlapPos)
                positions

        Vertical ->
            List.Extra.updateIfIndex
                (\idx ->
                    List.member
                        idx
                        (verticalOverlapIndices ship.size indices)
                )
                (always overlapPos)
                positions


placeShipPosition : Int -> Ship -> Position -> List Position -> List Position
placeShipPosition cols ship pos positions =
    let
        maybeIndex =
            List.Extra.elemIndex pos positions
    in
    case maybeIndex of
        Nothing ->
            positions

        Just index ->
            case ship.orientation of
                Vertical ->
                    placeVerticalShip index ship.size positions

                Horizontal ->
                    placeHorizontalShip cols index ship.size positions


placeVerticalShip : Int -> Int -> List Position -> List Position
placeVerticalShip index size positions =
    List.Extra.updateIfIndex
        (\idx ->
            List.member idx (verticalIndices index size)
        )
        (always takenPos)
        positions


placeHorizontalShip : Int -> Int -> Int -> List Position -> List Position
placeHorizontalShip cols index size positions =
    List.Extra.updateIfIndex
        (\idx ->
            List.member idx (horizontalIndices cols index size)
        )
        (always takenPos)
        positions


horizontalIndices : Int -> Int -> Int -> List Int
horizontalIndices cols index size =
    List.indexedMap
        (\i n -> n + (cols * i))
        (List.repeat size index)


verticalIndices : Int -> Int -> List Int
verticalIndices index size =
    List.indexedMap
        (\i n -> n + i)
        (List.repeat size index)


horizontalOverlapIndices : Int -> Int -> List Int -> List Int
horizontalOverlapIndices cols size indices =
    List.Extra.filterNot
        (\idx -> List.member idx indices)
        (List.Extra.unique
            (List.concat
                (List.map
                    (\index ->
                        List.indexedMap
                            (\i n -> n - (cols * i))
                            (List.repeat size index)
                    )
                    indices
                )
            )
        )


verticalOverlapIndices : Int -> List Int -> List Int
verticalOverlapIndices size indices =
    List.Extra.filterNot
        (\idx -> List.member idx indices)
        (List.Extra.unique
            (List.concat
                (List.map
                    (\index ->
                        List.indexedMap
                            (\i n -> n - i)
                            (List.repeat size index)
                    )
                    indices
                )
            )
        )
