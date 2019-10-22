module Main exposing (main)

import Bitwise
import Browser
import Debug
import Html exposing (..)
import Html.Attributes exposing (draggable, style)
import Html.Events exposing (onClick)
import List.Extra
import Random
import Random.Extra
import Random.List
import Ship
import Svg
import Svg.Attributes



-- CONSTANTS


boxSize =
    40


maxCol =
    10


maxRow =
    10


takenPos =
    ( -1, -1 )


boundaryPos =
    ( -2, -2 )


overlapPos =
    ( -3, -3 )



-- MAIN


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { ships : List Ship.Ship
    , availablePositions : List (List Ship.Position)
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] (Debug.log "grid" (initGrid maxRow maxCol))
    , Cmd.none
    )



-- UPDATE


type Msg
    = None
    | Start
    | SetOrientation
    | SetPosition
    | UpdateOrientation (List Ship.Orientation)
    | UpdatePosition (List Ship.Position)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        Start ->
            update
                SetOrientation
                { model | ships = initShips }

        SetOrientation ->
            ( model
            , Random.generate UpdateOrientation
                (randomizeOrientation model.ships)
            )

        UpdateOrientation orientations ->
            update
                SetPosition
                { model | ships = updateOrientation orientations model.ships }

        SetPosition ->
            ( model
            , Random.generate UpdatePosition
                (choosePositions model.ships (Debug.log "list" (List.concat model.availablePositions)))
            )

        UpdatePosition positions ->
            ( { model | ships = updatePositions (Debug.log "positions" positions) model.ships }
            , Cmd.none
            )



-- FUNCTIONS


initGrid : Int -> Int -> List (List Ship.Position)
initGrid rows cols =
    List.indexedMap fillGrid
        (List.repeat rows cols)


fillGrid : Int -> Int -> List Ship.Position
fillGrid index length =
    List.map
        (Tuple.pair index)
        (List.range 0 (length - 1))


initShips : List Ship.Ship
initShips =
    [ Ship.Ship Ship.Carrier 5 ( 0, 0 ) Ship.Vertical
    , Ship.Ship Ship.Battleship 4 ( 0, 0 ) Ship.Vertical
    , Ship.Ship Ship.Cruiser 3 ( 0, 0 ) Ship.Vertical
    , Ship.Ship Ship.Submarine 3 ( 0, 0 ) Ship.Vertical
    , Ship.Ship Ship.Destroyer 2 ( 0, 0 ) Ship.Vertical
    ]


updateOrientation : List Ship.Orientation -> List Ship.Ship -> List Ship.Ship
updateOrientation orientations ships =
    List.map2
        (\orientation ship -> { ship | orientation = orientation })
        orientations
        ships



-- SHIP RANDOM POSITIONING FUNCTIONS


randomizeOrientation : List Ship.Ship -> Random.Generator (List Ship.Orientation)
randomizeOrientation ships =
    Random.map
        (\list -> List.map mapToOrientation list)
        (Random.list (List.length ships) (Random.int 0 1))


mapToOrientation : Int -> Ship.Orientation
mapToOrientation n =
    if n == 0 then
        Ship.Horizontal

    else
        Ship.Vertical


choosePositions : List Ship.Ship -> List Ship.Position -> Random.Generator (List Ship.Position)
choosePositions ships availablePositions =
    let
        maybeShip =
            List.head ships
    in
    case maybeShip of
        Nothing ->
            Random.constant availablePositions

        Just ship ->
            Random.List.choose
                (List.filter
                    (\n -> Tuple.first n > -1)
                    (placeBoundaries
                        ship
                        (placeOverlapPositions ship availablePositions)
                    )
                )
                |> Random.andThen
                    (\res ->
                        let
                            maybePos =
                                Tuple.first res
                        in
                        case maybePos of
                            Nothing ->
                                Random.constant [ takenPos ]

                            Just pos ->
                                if List.length ships == 0 then
                                    Random.constant [ pos ]

                                else
                                    Random.map
                                        (\partialList ->
                                            pos :: partialList
                                        )
                                        (choosePositions
                                            (List.drop 1 ships)
                                            (placeShipPosition ship pos availablePositions)
                                        )
                    )


placeBoundaries : Ship.Ship -> List Ship.Position -> List Ship.Position
placeBoundaries ship positions =
    case ship.orientation of
        Ship.Horizontal ->
            List.Extra.updateIfIndex
                (byHorizontal ship.size)
                (\_ -> boundaryPos)
                positions

        Ship.Vertical ->
            List.Extra.updateIfIndex
                (byVertical ship.size)
                (\_ -> boundaryPos)
                positions


byHorizontal : Int -> Int -> Bool
byHorizontal size index =
    (maxRow * maxCol) - (size - 1) * maxCol - 1 < index


byVertical : Int -> Int -> Bool
byVertical size index =
    (maxRow - modBy 10 index) < size


placeOverlapPositions : Ship.Ship -> List Ship.Position -> List Ship.Position
placeOverlapPositions ship positions =
    let
        indices =
            List.Extra.elemIndices takenPos positions
    in
    case ship.orientation of
        Ship.Horizontal ->
            List.Extra.updateIfIndex
                (\idx ->
                    List.member
                        idx
                        (horizontalOverlapIndices ship.size indices)
                )
                (\_ -> overlapPos)
                positions

        Ship.Vertical ->
            List.Extra.updateIfIndex
                (\idx ->
                    List.member
                        idx
                        (verticalOverlapIndices ship.size indices)
                )
                (\_ -> overlapPos)
                positions


horizontalOverlapIndices : Int -> List Int -> List Int
horizontalOverlapIndices size indices =
    List.Extra.filterNot
        (\idx -> List.member idx indices)
        (List.Extra.unique
            (List.concat
                (List.map
                    (\index ->
                        List.indexedMap
                            (\i n -> n - (maxCol * i))
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


placeShipPosition : Ship.Ship -> Ship.Position -> List Ship.Position -> List Ship.Position
placeShipPosition ship pos positions =
    let
        maybeIndex =
            List.Extra.elemIndex pos positions
    in
    case maybeIndex of
        Nothing ->
            positions

        Just index ->
            case ship.orientation of
                Ship.Vertical ->
                    placeVerticalShip index ship.size positions

                Ship.Horizontal ->
                    placeHorizontalShip index ship.size positions


placeVerticalShip : Int -> Int -> List Ship.Position -> List Ship.Position
placeVerticalShip index size positions =
    List.Extra.updateIfIndex
        (\idx ->
            List.member idx (verticalIndices index size)
        )
        (\_ -> takenPos)
        positions


placeHorizontalShip : Int -> Int -> List Ship.Position -> List Ship.Position
placeHorizontalShip index size positions =
    List.Extra.updateIfIndex
        (\idx ->
            List.member idx (horizontalIndices index size)
        )
        (\_ -> takenPos)
        positions


horizontalIndices : Int -> Int -> List Int
horizontalIndices index size =
    List.indexedMap
        (\i n -> n + (maxCol * i))
        (List.repeat size index)


verticalIndices : Int -> Int -> List Int
verticalIndices index size =
    List.indexedMap
        (\i n -> n + i)
        (List.repeat size index)


randomizePositions : List Ship.Ship -> Random.Generator (List Ship.Position)
randomizePositions ships =
    let
        sizes =
            List.map mapToSizeTuple ships
    in
    Random.Extra.sequence (List.map mapToRandomTuplePair sizes)


mapToSizeTuple : Ship.Ship -> Ship.Position
mapToSizeTuple ship =
    if ship.orientation == Ship.Horizontal then
        ( ship.size, 0 )

    else
        ( 0, ship.size )


mapToRandomTuplePair : Ship.Position -> Random.Generator Ship.Position
mapToRandomTuplePair tuple =
    let
        cols =
            Tuple.first tuple

        rows =
            Tuple.second tuple
    in
    Random.pair
        (Random.map2
            pickFirstDigit
            (Random.int 0 (maxCol - 1 - cols))
            (Random.Extra.choice 0 1)
        )
        (Random.map2
            pickFirstDigit
            (Random.int 0 (maxRow - 1 - rows))
            (Random.Extra.choice 0 1)
        )


pickFirstDigit : Int -> Int -> Int
pickFirstDigit d flag =
    modBy 10 (2 * d + flag)


updatePositions : List Ship.Position -> List Ship.Ship -> List Ship.Ship
updatePositions positions ships =
    List.map2 updatePosition positions ships


updatePosition : Ship.Position -> Ship.Ship -> Ship.Ship
updatePosition pos ship =
    { ship | position = pos }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Battleship"
    , body =
        [ div []
            [ div [ style "margin-bottom" "1rem" ]
                [ button
                    [ onClick Start
                    ]
                    [ text "Start" ]
                ]
            , viewGrid
                model
                []
                (makeShips model.ships)
            , viewGrid model
                [ style "float" "right" ]
                []
            ]
        ]
    }


viewGrid : Model -> List (Html.Attribute msg) -> List (Svg.Svg msg) -> Html msg
viewGrid model attrs nodes =
    Svg.svg
        (List.concat
            [ [ Svg.Attributes.width (String.fromInt (maxCol * boxSize + 20))
              , Svg.Attributes.height (String.fromInt (maxRow * boxSize + 20))
              , Svg.Attributes.viewBox
                    (String.join
                        " "
                        [ "0 0"
                        , String.fromInt (maxCol * boxSize)
                        , String.fromInt (maxRow * boxSize)
                        ]
                    )
              ]
            , attrs
            ]
        )
        (List.concat
            [ [ box
              , verticalLines
              , horizontalLines
              ]
            , nodes
            ]
        )



-- FUNCTIONS


makeShips : List Ship.Ship -> List (Svg.Svg msg)
makeShips ships =
    List.map svgShip ships


shipSize : Ship.Orientation -> Ship.Ship -> Int
shipSize orientation ship =
    if ship.orientation == orientation then
        ship.size * boxSize

    else
        boxSize



-- SHAPES


svgShip : Ship.Ship -> Svg.Svg msg
svgShip ship =
    Svg.g
        [ draggable "true"
        , style "cursor" "move"
        ]
        [ Svg.rect
            [ Svg.Attributes.x
                (String.fromInt (Tuple.first ship.position * boxSize))
            , Svg.Attributes.y
                (String.fromInt (Tuple.second ship.position * boxSize))
            , Svg.Attributes.width
                (String.fromInt (shipSize Ship.Horizontal ship))
            , Svg.Attributes.height
                (String.fromInt (shipSize Ship.Vertical ship))
            , Svg.Attributes.fill "green"
            , Svg.Attributes.fillOpacity "0.7"
            , Svg.Attributes.stroke "green"
            , Svg.Attributes.strokeWidth "2px"
            ]
            []
        ]


box : Svg.Svg msg
box =
    Svg.rect
        [ Svg.Attributes.x "0"
        , Svg.Attributes.y "0"
        , Svg.Attributes.width (String.fromInt (maxCol * boxSize))
        , Svg.Attributes.height (String.fromInt (maxRow * boxSize))
        , Svg.Attributes.fill "transparent"
        , Svg.Attributes.stroke "black"
        , Svg.Attributes.strokeWidth "5px"
        ]
        []


verticalLines : Svg.Svg msg
verticalLines =
    let
        lines =
            (boxSize * maxRow) // boxSize
    in
    Svg.g []
        (List.map
            makeVerticalLine
            (List.range 1 lines)
        )


horizontalLines : Svg.Svg msg
horizontalLines =
    let
        lines =
            (boxSize * maxCol) // boxSize
    in
    Svg.g []
        (List.map
            makeHorizontalLine
            (List.range 1 lines)
        )


makeVerticalLine : Int -> Svg.Svg msg
makeVerticalLine pos =
    let
        x =
            pos * boxSize
    in
    makeLine x 0 x (boxSize * maxRow)


makeHorizontalLine : Int -> Svg.Svg msg
makeHorizontalLine pos =
    let
        y =
            pos * boxSize
    in
    makeLine 0 y (boxSize * maxCol) y


makeLine : Int -> Int -> Int -> Int -> Svg.Svg msg
makeLine x1 y1 x2 y2 =
    Svg.line
        [ Svg.Attributes.x1 (String.fromInt x1)
        , Svg.Attributes.y1 (String.fromInt y1)
        , Svg.Attributes.x2 (String.fromInt x2)
        , Svg.Attributes.y2 (String.fromInt y2)
        , Svg.Attributes.stroke "black"
        , Svg.Attributes.strokeWidth "1px"
        ]
        []
