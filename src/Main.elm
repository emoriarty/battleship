module Main exposing (main)

import Browser
import Debug
import Html exposing (..)
import Html.Attributes exposing (draggable, style)
import Html.Events exposing (onClick)
import Json.Decode as Json
import List.Extra
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


noShipIndex =
    -1



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
    , shipIndex : Int
    , draggedShip : Ship.Ship
    , offset : OffsetDrag
    }


type alias OffsetDrag =
    { x : Int
    , y : Int
    }


type alias StartDrag =
    { shipIndex : Int
    , offset : OffsetDrag
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] noShipIndex Ship.noShip (OffsetDrag 0 0)
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
    | DragStart StartDrag
    | Drag OffsetDrag
    | DragEnd


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
                (Ship.randomizePositions model.ships (initGrid maxRow maxCol))
            )

        UpdatePosition positions ->
            ( { model | ships = updatePositions positions model.ships }
            , Cmd.none
            )

        DragStart startDrag ->
            let
                maybeShip =
                    List.Extra.getAt startDrag.shipIndex model.ships
            in
            case maybeShip of
                Just ship ->
                    ( { model
                        | shipIndex = startDrag.shipIndex
                        , draggedShip = ship
                        , offset = encodeOffset ship.position startDrag.offset
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        Drag pointer ->
            ( { model | ships = updateShips pointer model.offset model.draggedShip model.ships model.shipIndex }
            , Cmd.none
            )

        DragEnd ->
            ( { model | shipIndex = noShipIndex, draggedShip = Ship.noShip }
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
        (Ship.Position index)
        (List.range 0 (length - 1))


initShips : List Ship.Ship
initShips =
    [ Ship.Ship Ship.Carrier 5 (Ship.Position 0 0) Ship.Vertical
    , Ship.Ship Ship.Battleship 4 (Ship.Position 0 0) Ship.Vertical
    , Ship.Ship Ship.Cruiser 3 (Ship.Position 0 0) Ship.Vertical
    , Ship.Ship Ship.Submarine 3 (Ship.Position 0 0) Ship.Vertical
    , Ship.Ship Ship.Destroyer 2 (Ship.Position 0 0) Ship.Vertical
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


updateShips : OffsetDrag -> OffsetDrag -> Ship.Ship -> List Ship.Ship -> Int -> List Ship.Ship
updateShips pointer offset ship ships index =
    let
        updatedShip =
            updateShipPosition pointer offset ship
    in
    List.Extra.setAt index updatedShip ships


updateShipPosition : OffsetDrag -> OffsetDrag -> Ship.Ship -> Ship.Ship
updateShipPosition gridPos offset ship =
    let
        x =
            updateX ship gridPos.x offset.x

        y =
            updateY ship gridPos.y offset.y

        shipPosition =
            ship.position
    in
    { ship | position = { shipPosition | x = x, y = y } }


updateX : Ship.Ship -> Int -> Int -> Int
updateX ship pointerX shipOffset =
    let
        x =
            pointerX // boxSize - shipOffset

        maxAllowedX =
            case ship.orientation of
                Ship.Vertical ->
                    maxCol

                Ship.Horizontal ->
                    maxCol - ship.size + 1

                Ship.NoOrientation ->
                    maxCol
    in
    if x < maxAllowedX && x > -1 then
        x

    else
        ship.position.x


updateY : Ship.Ship -> Int -> Int -> Int
updateY ship pointerY shipOffset =
    let
        y =
            pointerY // boxSize - shipOffset

        maxAllowedY =
            case ship.orientation of
                Ship.Vertical ->
                    maxRow - ship.size + 1

                Ship.Horizontal ->
                    maxRow

                Ship.NoOrientation ->
                    maxRow
    in
    if y < maxAllowedY && y > -1 then
        y

    else
        ship.position.y


encodeOffset : Ship.Position -> OffsetDrag -> OffsetDrag
encodeOffset position offset =
    let
        offsetX =
            offset.x // boxSize - position.x

        offsetY =
            offset.y // boxSize - position.y
    in
    { x = offsetX, y = offsetY }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTML.EVENTS


onDragStart : Attribute Msg
onDragStart =
    Html.Events.on "mousedown" (Json.map DragStart startDecoder)


onDrag : Attribute Msg
onDrag =
    Html.Events.on "mousemove" (Json.map Drag positionDecoder)


onDragEnd : Attribute Msg
onDragEnd =
    Html.Events.onMouseUp DragEnd


onLeaveDrag : Attribute Msg
onLeaveDrag =
    Html.Events.onMouseLeave DragEnd



-- DECODERS


startDecoder : Json.Decoder StartDrag
startDecoder =
    Json.map2 StartDrag idDecoder positionDecoder


positionDecoder : Json.Decoder OffsetDrag
positionDecoder =
    Json.map2 OffsetDrag
        (Json.at [ "offsetX" ] Json.int)
        (Json.at [ "offsetY" ] Json.int)


idDecoder : Json.Decoder Int
idDecoder =
    Json.at [ "target", "id" ] Json.string
        |> Json.andThen stringToIntDecoder


stringToIntDecoder : String -> Json.Decoder Int
stringToIntDecoder string =
    String.toInt string
        |> Maybe.map Json.succeed
        |> Maybe.withDefault
            (Json.fail <|
                "The provided String '"
                    ++ string
                    ++ "' is not a mumber"
            )



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
                [ onDrag
                , onDragStart
                , onDragEnd
                , onLeaveDrag
                ]
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


makeShips : List Ship.Ship -> List (Svg.Svg Msg)
makeShips ships =
    List.indexedMap svgShip ships


shipSize : Ship.Orientation -> Ship.Ship -> Int
shipSize orientation ship =
    if ship.orientation == orientation then
        ship.size * boxSize

    else
        boxSize



-- SHAPES


svgShip : Int -> Ship.Ship -> Svg.Svg Msg
svgShip index ship =
    Svg.g
        [ style "cursor" "move"
        ]
        [ Svg.rect
            [ Svg.Attributes.id (String.fromInt index)
            , Svg.Attributes.x
                (String.fromInt (ship.position.x * boxSize))
            , Svg.Attributes.y
                (String.fromInt (ship.position.y * boxSize))
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
