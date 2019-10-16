module Main exposing (main)

import Bitwise
import Browser
import Debug
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List.Extra
import Random
import Random.Extra
import Random.List
import Svg
import Svg.Attributes



-- CONSTANTS


boxSize =
    40


maxCol =
    10


maxRow =
    10



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
    { ships : List Ship
    , availablePositions : List (List Position)
    }


type alias Ship =
    { class : ShipType
    , size : Int
    , position : Point
    , orientation : Orientation
    }


type alias Position =
    ( Int, Int )


type ShipType
    = Destroyer
    | Submarine
    | Cruiser
    | Battleship
    | Carrier


type Orientation
    = Horizontal
    | Vertical


type alias Point =
    { x : Int, y : Int }



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
    | UpdateOrientation (List Orientation)
    | UpdatePosition (List Position)


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


initGrid : Int -> Int -> List (List Position)
initGrid rows cols =
    List.indexedMap fillGrid
        (List.repeat rows cols)


fillGrid : Int -> Int -> List Position
fillGrid index length =
    List.map
        (Tuple.pair index)
        (List.range 0 (length - 1))


initShips : List Ship
initShips =
    [ Ship Carrier 5 (Point 0 0) Vertical
    , Ship Battleship 4 (Point 0 0) Vertical
    , Ship Cruiser 3 (Point 0 0) Vertical
    , Ship Submarine 3 (Point 0 0) Vertical
    , Ship Destroyer 2 (Point 0 0) Vertical
    ]



--    , Ship Cruiser 3 (Point 0 0) Vertical
--   , Ship Submarine 3 (Point 0 0) Vertical
--   , Ship Destroyer 2 (Point 0 0) Vertical


randomizeOrientation : List Ship -> Random.Generator (List Orientation)
randomizeOrientation ships =
    Random.constant [ Horizontal, Horizontal, Horizontal, Horizontal, Horizontal ]



--    Random.map
--        (\list -> List.map mapToOrientation list)
--        (Random.list (List.length ships) (Random.int 0 1))


mapToOrientation : Int -> Orientation
mapToOrientation n =
    if n == 0 then
        Horizontal

    else
        Vertical


updateOrientation : List Orientation -> List Ship -> List Ship
updateOrientation orientations ships =
    List.map2
        (\orientation ship -> { ship | orientation = orientation })
        orientations
        ships



-- Not only removing positions by size to prevent overflow, but also remove -1 positions


choosePositions : List Ship -> List Position -> Random.Generator (List Position)
choosePositions ships availablePositions =
    let
        maybeShip =
            List.head (Debug.log ">>> SHIPS" ships)
    in
    case maybeShip of
        Nothing ->
            Random.constant availablePositions

        Just ship ->
            Random.List.choose
                (List.filter
                    (\n -> Tuple.first n > -1)
                    (Debug.log "Ships removed"
                        (dropOverlapPositions
                            ship
                            (Debug.log "Boundaries removed" (dropBoundaries ship (Debug.log "Positions" availablePositions)))
                        )
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
                                Random.constant [ ( -1, -1 ) ]

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
                                            (Debug.log "Place ship position" (placeShipPosition ship (Debug.log "Position" pos) availablePositions))
                                        )
                    )


dropBoundaries : Ship -> List Position -> List Position
dropBoundaries ship positions =
    case ship.orientation of
        Horizontal ->
            List.Extra.removeIfIndex
                (byHorizontal ship.size)
                positions

        Vertical ->
            List.filter (byVertical ship.size) positions


byHorizontal : Int -> Int -> Bool
byHorizontal size index =
    (maxRow * maxCol) - (size - 1) * maxCol - 1 < index


byVertical : Int -> Position -> Bool
byVertical size position =
    size - maxRow >= Tuple.second position


dropOverlapPositions : Ship -> List Position -> List Position
dropOverlapPositions ship positions =
    let
        indices =
            Debug.log "ship indices" (List.Extra.elemIndices ( -1, -1 ) positions)
    in
    case ship.orientation of
        Horizontal ->
            List.Extra.filterNot
                ((==) ( -1, -1 ))
                (Debug.log
                    "Horizontal overlap dropping"
                    (List.Extra.updateIfIndex
                        (\idx ->
                            List.member
                                idx
                                (Debug.log "horizontalOverlapIndices"
                                    (List.Extra.unique
                                        (List.concat
                                            (List.map
                                                (horizontalOverlapIndices ship.size)
                                                indices
                                            )
                                        )
                                    )
                                )
                        )
                        (\_ -> ( -1, -1 ))
                        positions
                    )
                )

        Vertical ->
            List.Extra.filterNot
                ((==) ( -1, -1 ))
                (Debug.log "Vertical overlap dropping"
                    (List.Extra.updateIfIndex
                        (\idx ->
                            List.member
                                idx
                                (List.Extra.unique
                                    (List.concat
                                        (List.map
                                            (filterVerticalOverlapPositions ship)
                                            indices
                                        )
                                    )
                                )
                        )
                        (\_ -> ( -1, -1 ))
                        positions
                    )
                )


horizontalOverlapIndices : Int -> Int -> List Int
horizontalOverlapIndices size index =
    List.indexedMap
        (\i n -> n - (maxCol * i))
        (List.repeat size index)


filterVerticalOverlapPositions : Ship -> Int -> List Int
filterVerticalOverlapPositions ship idx =
    let
        col =
            idx // maxRow
    in
    List.filter
        ((>) 0)
        (List.map
            ((-) maxRow)
            (List.repeat idx ship.size)
        )


placeShipPosition : Ship -> Position -> List Position -> List Position
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
                Vertical ->
                    placeVerticalShip index pos ship positions

                Horizontal ->
                    placeHorizontalShip index ship.size positions


placeVerticalShip : Int -> Position -> Ship -> List Position -> List Position
placeVerticalShip index pos ship positions =
    let
        x =
            Tuple.first pos

        y =
            Tuple.second pos

        topOffset =
            y // y

        bottomOffset =
            (y + ship.size - 1) // (maxCol - 1)

        leftOffset =
            (x // x) * maxCol

        rightOffset =
            (*) (modBy maxCol (x + 1)) maxCol // (x + 1)
    in
    List.Extra.updateIfIndex
        (\idx ->
            List.member
                idx
                (List.Extra.unique
                    (List.concat
                        [ List.range
                            (index - topOffset)
                            (index + (ship.size - bottomOffset))

                        --                        , List.range
                        --                           (index - topOffset - leftOffset)
                        --                          (index + (ship.size - bottomOffset) - leftOffset)
                        --                      , List.range
                        --                         (index - topOffset + rightOffset)
                        --                         (index + (ship.size - bottomOffset) + rightOffset)
                        ]
                    )
                )
        )
        (\_ -> ( -1, -1 ))
        positions


placeHorizontalShip : Int -> Int -> List Position -> List Position
placeHorizontalShip index size positions =
    List.Extra.updateIfIndex
        (\idx ->
            List.member idx (horizontalIndices index size)
        )
        (\_ -> ( -1, -1 ))
        positions


horizontalIndices : Int -> Int -> List Int
horizontalIndices index size =
    List.indexedMap
        (\i n -> n + (maxCol * i))
        (List.repeat size index)



-- removeSurrondingArea : Int -> List Position -> List Position


randomizePositions : List Ship -> Random.Generator (List Position)
randomizePositions ships =
    let
        sizes =
            List.map mapToSizeTuple ships
    in
    Random.Extra.sequence (List.map mapToRandomTuplePair sizes)


mapToSizeTuple : Ship -> Position
mapToSizeTuple ship =
    if ship.orientation == Horizontal then
        ( ship.size, 0 )

    else
        ( 0, ship.size )


mapToRandomTuplePair : Position -> Random.Generator Position
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


updatePositions : List Position -> List Ship -> List Ship
updatePositions positions ships =
    List.map2 updatePosition positions ships


updatePosition : Position -> Ship -> Ship
updatePosition point ship =
    let
        x =
            Tuple.first point

        y =
            Tuple.second point
    in
    { ship | position = Point x y }



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
            [ [ Svg.Attributes.width "420"
              , Svg.Attributes.height "420"
              , Svg.Attributes.viewBox "0 0 400 400"
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


makeShips : List Ship -> List (Svg.Svg msg)
makeShips ships =
    List.map svgShip ships


shipSize : Orientation -> Ship -> Int
shipSize orientation ship =
    if ship.orientation == orientation then
        ship.size * boxSize

    else
        boxSize



-- SHAPES


svgShip : Ship -> Svg.Svg msg
svgShip ship =
    Svg.g []
        [ Svg.rect
            [ Svg.Attributes.x
                (String.fromInt (ship.position.x * boxSize))
            , Svg.Attributes.y
                (String.fromInt (ship.position.y * boxSize))
            , Svg.Attributes.width
                (String.fromInt (shipSize Horizontal ship))
            , Svg.Attributes.height
                (String.fromInt (shipSize Vertical ship))
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
        , Svg.Attributes.width "400"
        , Svg.Attributes.height "400"
        , Svg.Attributes.fill "transparent"
        , Svg.Attributes.stroke "black"
        , Svg.Attributes.strokeWidth "5px"
        ]
        []


verticalLines : Svg.Svg msg
verticalLines =
    Svg.g []
        [ Svg.line
            [ Svg.Attributes.x1 "40"
            , Svg.Attributes.y1 "0"
            , Svg.Attributes.x2 "40"
            , Svg.Attributes.y1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        , Svg.line
            [ Svg.Attributes.x1 "80"
            , Svg.Attributes.y1 "0"
            , Svg.Attributes.x2 "80"
            , Svg.Attributes.y1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        , Svg.line
            [ Svg.Attributes.x1 "120"
            , Svg.Attributes.y1 "0"
            , Svg.Attributes.x2 "120"
            , Svg.Attributes.y1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        , Svg.line
            [ Svg.Attributes.x1 "160"
            , Svg.Attributes.y1 "0"
            , Svg.Attributes.x2 "160"
            , Svg.Attributes.y1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        , Svg.line
            [ Svg.Attributes.x1 "200"
            , Svg.Attributes.y1 "0"
            , Svg.Attributes.x2 "200"
            , Svg.Attributes.y1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        , Svg.line
            [ Svg.Attributes.x1 "240"
            , Svg.Attributes.y1 "0"
            , Svg.Attributes.x2 "240"
            , Svg.Attributes.y1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        , Svg.line
            [ Svg.Attributes.x1 "280"
            , Svg.Attributes.y1 "0"
            , Svg.Attributes.x2 "280"
            , Svg.Attributes.y1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        , Svg.line
            [ Svg.Attributes.x1 "320"
            , Svg.Attributes.y1 "0"
            , Svg.Attributes.x2 "320"
            , Svg.Attributes.y1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        , Svg.line
            [ Svg.Attributes.x1 "360"
            , Svg.Attributes.y1 "0"
            , Svg.Attributes.x2 "360"
            , Svg.Attributes.y1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        ]


horizontalLines : Svg.Svg msg
horizontalLines =
    Svg.g []
        [ Svg.line
            [ Svg.Attributes.y1 "40"
            , Svg.Attributes.x1 "0"
            , Svg.Attributes.y2 "40"
            , Svg.Attributes.x1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        , Svg.line
            [ Svg.Attributes.y1 "80"
            , Svg.Attributes.x1 "0"
            , Svg.Attributes.y2 "80"
            , Svg.Attributes.x1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        , Svg.line
            [ Svg.Attributes.y1 "120"
            , Svg.Attributes.x1 "0"
            , Svg.Attributes.y2 "120"
            , Svg.Attributes.x1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        , Svg.line
            [ Svg.Attributes.y1 "160"
            , Svg.Attributes.x1 "0"
            , Svg.Attributes.y2 "160"
            , Svg.Attributes.x1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        , Svg.line
            [ Svg.Attributes.y1 "200"
            , Svg.Attributes.x1 "0"
            , Svg.Attributes.y2 "200"
            , Svg.Attributes.x1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        , Svg.line
            [ Svg.Attributes.y1 "240"
            , Svg.Attributes.x1 "0"
            , Svg.Attributes.y2 "240"
            , Svg.Attributes.x1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        , Svg.line
            [ Svg.Attributes.y1 "280"
            , Svg.Attributes.x1 "0"
            , Svg.Attributes.y2 "280"
            , Svg.Attributes.x1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        , Svg.line
            [ Svg.Attributes.y1 "320"
            , Svg.Attributes.x1 "0"
            , Svg.Attributes.y2 "320"
            , Svg.Attributes.x1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        , Svg.line
            [ Svg.Attributes.y1 "360"
            , Svg.Attributes.x1 "0"
            , Svg.Attributes.y2 "360"
            , Svg.Attributes.x1 "400"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1px"
            ]
            []
        ]
