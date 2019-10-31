module Main exposing (main)

import Browser
import Debug
import Html exposing (..)
import Html.Attributes exposing (draggable, style)
import Html.Events exposing (onClick)
import Random
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
                (Ship.randomizeOrientation model.ships)
            )

        UpdateOrientation orientations ->
            update
                SetPosition
                { model | ships = updateOrientation orientations model.ships }

        SetPosition ->
            ( model
            , Random.generate UpdatePosition
                (Ship.randomizePositions model.ships model.availablePositions)
            )

        UpdatePosition positions ->
            ( { model | ships = updatePositions positions model.ships }
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
